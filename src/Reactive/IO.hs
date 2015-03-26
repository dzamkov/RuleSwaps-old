{-# LANGUAGE MultiParamTypeClasses #-}
module Reactive.IO (
    Event (..),
    never,
    union,
    mapE,
    filterJust,
    memoE,
    newEvent,
    registerEternal,
    registerSafeEternal,
    await,
    Behavior (..),
    mapB,
    lift2,
    tag,
    accumB,
    changes,
    memoB,
    newBehavior,
    registerBehaviorEternal
) where

import Prelude hiding (map)
import Reactive (Reactive, ReactiveState, ReactiveDiscrete)
import qualified Reactive
import Data.IORef
import System.IO.Unsafe
import System.Mem.Weak
import Data.Monoid
import Control.Concurrent.MVar
import Control.Monad.State
import Control.Applicative

-- | Identifies an event in an IO-based reactive system.
data Event a = Event {

    -- | Registers an event handler to be invoked the next time this event
    -- occurs. It is safe to call this while the event is being processed,
    -- in which case, the handler will be invoked on the next occurence.
    -- Event handlers will be invoked in the order they were registered.
    register :: (a -> IO ()) -> IO () }

instance Monoid (Event a) where
    mempty = never
    mappend x y = memoE $ union x y
instance Functor Event where
    fmap f = memoE . mapE f
instance Reactive.Event Event where
    filterJust = filterJust
-- TODO: EventSubdivide implementation

-- | An event that never occurs.
never :: Event a
never = Event $ const $ return ()

-- | Combines events without memoization.
union :: Event a -> Event a -> Event a
union x y = Event $ \h -> register x h >> register y h

-- | Maps an event without memoization.
mapE :: (a -> b) -> Event a -> Event b
mapE f x = Event $ register x . (. f)

-- | Filters the occurences of an event.
filterJust :: Event (Maybe a) -> Event a
filterJust x = Event $ \h -> register x $ \value -> case value of
    Just value -> h value
    Nothing -> return ()

-- | Caches the values for an event when they are generated for the first time.
memoE :: Event a -> Event a
memoE source = source -- TODO

-- | Begins executing the procedures given by an event.
executeIO :: Event (IO a) -> IO (Event a)
executeIO source = do
    (res, fire) <- newEvent
    register source $ \proc -> proc >>= fire
    return res

-- | Constructs a new event that can be manually fired by invoking the returned
-- procedure, which is not thread-safe and should have at most one instance
-- running at a time.
newEvent :: IO (Event a, a -> IO ())
newEvent = do
    ref <- newIORef []
    let register h = atomicModifyIORef ref $ \l -> (h : l, ())
        fire val = do
            handlers <- atomicModifyIORef ref $ \l -> ([], l)
            forM_ (reverse handlers) (\h -> h val)
    return (Event register, fire)

-- | Registers an event handler to be invoked /every/ time there is an
-- occurence of the given event, starting after this procedure returns.
registerEternal :: Event a -> (a -> IO ()) -> IO ()
registerEternal event handler =
    register event (\value -> handler value >> registerEternal event handler)

-- | Executes the given procedure and registers a handler for the given event.
-- All occurences of the event that happen during or after the execution of the
-- procedure are guranteed to be seen by the handler. The handler will not
-- be invoked until after execution is finished.
registerSafeEternal :: Event a -> (a -> IO ()) -> IO b -> IO b
registerSafeEternal event handler proc = do
    accumRef <- newIORef True -- is the procedure currently running?
    backlogRef <- newIORef [] -- backlog of unhandled occurences
    let process value = do
        accum <- readIORef accumRef
        when accum $ do
            modifyIORef backlogRef (value :)
            register event process
        unless accum $ do
            backlog <- readIORef backlogRef
            forM_ (reverse backlog) handler
            handler value
            registerEternal event handler
    register event process
    res <- proc
    writeIORef accumRef False
    return res

-- | Waits until there is a single occurence of the given event, returning
-- its value.
await :: Event a -> IO a
await event = do
    var <- newEmptyMVar
    register event $ putMVar var
    readMVar var

-- | Identifies a behavior in an IO-based reactive system.
data Behavior a = Behavior {

    -- | Gets the current value for this behavior.
    value :: IO a,

    -- | Registers a handler to be called the next time this behavior is
    -- invalidated, that is, when its value has potentially changed. While
    -- the handler is being invoked, the old value for the behavior is
    -- still available. The handler returns a procedure to be invoked after
    -- all invalidations have been fully propogated. In this procedure,
    -- the new value for the behavior is available.
    registerInvalidate :: IO (IO ()) -> IO () }

instance Functor Behavior where
    fmap f x = memoB $ mapB f x
instance Applicative Behavior where
    pure x = Behavior {
        value = return x,
        registerInvalidate = const $ return () }
    (<*>) f x = memoB $ lift2 id f x
instance Reactive Event Behavior where
    (<@>) f x = memoE $ tag id f x
instance ReactiveState IO Event Behavior where
    accumB = accumB
    sample = value
    execute = unsafePerformIO . executeIO
instance ReactiveDiscrete Event Behavior where
    changes = memoE . changes

-- | Maps a behavior without memoization.
mapB :: (a -> b) -> Behavior a -> Behavior b
mapB f source = source { value = f <$> value source }

-- | Lifts a function to behaviors without memoization.
lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 f x y = Behavior {
    value = f <$> value x <*> value y,
    registerInvalidate = \h ->
        registerInvalidate x h >> registerInvalidate y h }

-- | Tags an event with the value of a behavior at the time it occurs,
-- without memoization.
tag :: (a -> b -> c) -> Behavior a -> Event b -> Event c
tag f x y = Event $ \h -> register y $ \val -> do
    cur <- value x
    h (f cur val)

-- | A mutex used to insure that only one invalidation propogation is ongoing
-- in a behavior network at any given time. This is needed for the
-- implementation of 'changes', which must be able to get the old and new
-- values for a behavior due to the effects of a single source event. Without
-- this lock, the effects of multiple simultaneous event occurences
-- may get mixed together in the values produced by 'changes'.
globalBehaviorLock :: MVar ()
{-# NOINLINE globalBehaviorLock #-}
globalBehaviorLock = unsafePerformIO newEmptyMVar

-- TODO: find a smarter way to implement 'changes' that doesn't involve a
-- global lock.

-- | Constructs a new handlers list for 'registerInvalidate'. Returns
-- the appropriate @registerInvalidate@ and @invalidate@ procedures.
newHandlersList :: IO (IO (IO ()) -> IO (), IO (IO ()))
newHandlersList = do
    handlersRef <- newIORef []
    let invalidate = do
            handlers <- atomicModifyIORef handlersRef $ \l -> ([], l)
            actions <- forM (reverse handlers) id
            return $ forM_ actions id
        registerInvalidate handler = modifyIORef handlersRef (handler :)
    return (registerInvalidate, invalidate)

-- | Constructs a behavior with the given initial value that changes in
-- response to an event, without memoization.
accumB :: a -> Event (a -> a) -> IO (Behavior a)
accumB value event = do
    ref <- newIORef value
    weakRef <- mkWeakPtr ref Nothing
    (registerInvalidate', invalidate) <- newHandlersList
    let handler f = do
        ref <- deRefWeak weakRef
        case ref of
            Just ref -> do
                register event handler
                putMVar globalBehaviorLock ()
                action <- invalidate
                modifyIORef ref f
                action
                takeMVar globalBehaviorLock
            Nothing -> return ()
    register event handler
    return Behavior {
        value = readIORef ref,
        registerInvalidate = registerInvalidate' }

-- | Constructs an event which occurs when the given behavior changes,
-- without memoization.
changes :: Behavior a -> Event (a, a)
changes source = Event $ \handler ->
    registerInvalidate source $ do
        old <- value source
        return $ do
            new <- value source
            handler (old, new)

-- | Caches the values for a behavior when they are generated for the first
-- time.
memoB :: Behavior a -> Behavior a
memoB source = unsafePerformIO $ do
    ref <- newIORef Nothing
    weakRef <- mkWeakPtr ref Nothing
    (registerInvalidate', invalidate) <- newHandlersList
    let sourceInvalidate = do
        ref <- deRefWeak weakRef
        case ref of
            Just ref -> do
                registerInvalidate source sourceInvalidate
                val <- readIORef ref
                case val of
                    Just _ -> do
                        action <- invalidate
                        writeIORef ref Nothing
                        return action
                    Nothing -> return $ return () -- No change
            Nothing -> return $ return ()
    registerInvalidate source sourceInvalidate
    return Behavior {
        value = do
            val <- readIORef ref -- TODO: possible race condition
            case val of
                Just val -> return val
                Nothing -> do
                    val <- value source
                    writeIORef ref $ Just val
                    return val,
        registerInvalidate = registerInvalidate' }

-- | Constructs a new behavior that can be manually changed by invoking the
-- returned procedure.
newBehavior :: a -> IO (Behavior a, (a -> a) -> IO ())
newBehavior initial = do
    (event, change) <- newEvent
    behavior <- Reactive.accumB initial event
    return (behavior, change)

-- | Registers a procedure to be called every time the given behavior
-- changes (as determined by '=='), and initially, right after registration.
registerBehaviorEternal :: (Eq a) => Behavior a -> (a -> IO ()) -> IO ()
registerBehaviorEternal source handle = do
    let handleOnDiff (x, y) | x == y = return ()
        handleOnDiff (_, y) = handle y
    registerSafeEternal (changes source) handleOnDiff $ value source >>= handle
