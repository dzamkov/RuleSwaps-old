{-# LANGUAGE MultiParamTypeClasses #-}
module Reactive.IO where

import Prelude hiding (map)
import Reactive (Reactive, ReactiveState, ReactiveDiscrete)
import qualified Reactive
import Data.IORef
import System.IO.Unsafe
import System.Mem.Weak
import Data.Monoid
import Control.Monad.State
import Control.Applicative

-- | Describes a handler for an event, a procedure which first determines
-- whether the handler is still listening (returning 'Nothing' otherwise),
-- then returning a continuation which responds to the actual event.
type Handler a = IO (Maybe (a -> IO ()))

-- | Identifies an event in an IO-based reactive system.
data Event a = Event {

    -- | Indicates whether the event has occured yet.
    active :: IO Bool,

    -- | Registers an event handler to be invoked when this event occurs.
    register :: Handler a -> IO () }

instance Monoid (Event a) where
    mempty = Event {
        active = return False,
        register = const $ return () }
    mappend x y = Event {
        active = (||) <$> active x <*> active y,
        register = \h -> register x h >> register y h }
instance Functor Event where
    fmap f x = Event {
        active = active x,
        register = \h -> register x $ ((. f) <$>) <$> h }
instance Reactive.Event Event where
    filterJust x = Event {
        active = active x,
        register = \h -> register x $ (maybe (return ()) <$>) <$> h }

-- | Constructs a new event that can be manually fired by invoking the returned
-- procedure.
newEvent :: IO (Event a, a -> IO ())
newEvent = do
    ref <- newIORef ([], False, Nothing)
    let active = (\(_, a, _) -> a) <$> readIORef ref
    let register h = atomicModifyIORef ref $
            \(l, a, q) -> ((h : l, a, q), ())
    let process :: a -> [Handler a] -> IO [Handler a]
        process val listeners = flip evalStateT [] $ do
            forM_ listeners $ \listener' -> do
                listener <- lift listener'
                case listener of
                    Just listener -> do
                        lift $ listener val
                        modify (listener' :)
                    Nothing -> return () -- Inactive listener
            get -- Return remaining handlers
    let processQueue nListeners = do
        act <- atomicModifyIORef ref $ \(l, _, q) -> case q of
            Just [] -> ((nListeners ++ l, True, Nothing), Nothing)
            Just (val : tail) -> (([], True, Just tail),
                Just (nListeners ++ l, val))
            Nothing -> undefined -- Should never happen
        case act of
            Just (listeners, val) -> do
                nListeners <- process val listeners
                processQueue nListeners
            Nothing -> return ()
    let fire val = do
        listeners <- atomicModifyIORef ref $ \(l, _, q) -> case q of
            Nothing -> (([], True, Just []), Just l)
            Just q -> ((l, True, Just (q ++ [val])), Nothing)
        case listeners of
            Just listeners -> do
                nListeners <- process val listeners
                processQueue nListeners
            Nothing -> return ()
    return (Event {
        active = active,
        register = register }, fire)

-- | Registers an eternal event handler to be invoked when the given event
-- occurs. The handler may not be unregistered.
registerEternal :: Event a -> (a -> IO ()) -> IO ()
registerEternal event = register event . return . Just

-- | Identifies a behavior in an IO-based reactive system.
data Behavior a = Behavior {

    -- | Gets the current value for this behavior.
    value :: IO a,

    -- | An "event" that occurs when this behavior is invalidated, but before
    -- the change has been fully propogated through the behavior network.
    -- This is not a proper event because the network is in an invalid state
    -- when it occurs. The procedure provided by the event can be used to
    -- register a one-time procedure to be invoked when invalidation has
    -- been fully propogated.
    invalidated :: Event (IO () -> IO ()) }

instance Functor Behavior where
    fmap f x = memo $ map f x
instance Applicative Behavior where
    pure x = Behavior {
        value = return x,
        invalidated = mempty }
    (<*>) f x = memo $ lift2 id f x
instance Reactive Event Behavior where
    (<@>) f x = Event {
        active = active x,
        register = \h ->
            let apply inner val = value f >>= \cur -> inner (cur val)
            in register x $ (apply <$>) <$> h }
instance ReactiveState Event Behavior where
    accumB value event = unsafePerformIO $ do
        ref <- newIORef value
        weakRef <- mkWeakPtr ref Nothing
        (invalidated, invalidate) <- newEvent
        register event $ do
            ref <- deRefWeak weakRef
            case ref of
                Just ref -> return $ Just $ \f -> do
                    atomicModifyIORef ref (\value -> (f value, ()))
                    callAfterRef <- newIORef []
                    let callAfter f = atomicModifyIORef callAfterRef
                          (\listeners -> (f : listeners, ()))
                    invalidate callAfter
                    callAfter <- readIORef callAfterRef
                    forM_ callAfter id
                Nothing -> return Nothing
        isActive <- active event
        when isActive $ error "History not available"
        return Behavior {
            value = readIORef ref,
            invalidated = invalidated }
instance ReactiveDiscrete Event Behavior where
    changes x = Event {
        active = active (invalidated x),
        register = \h ->
            let listen inner callAfter = callAfter $ value x >>= inner
            in register (invalidated x) ((listen <$>) <$> h) }

-- | Maps a behavior without memoization.
map :: (a -> b) -> Behavior a -> Behavior b
map f x = Behavior {
    value = f <$> value x,
    invalidated = invalidated x }

-- | Lifts a function to behaviors without memoization.
lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 f x y = Behavior {
    value = f <$> value x <*> value y,
    invalidated = invalidated x <> invalidated y }

-- | Caches the values for a behavior when they are generated for the first
-- time.
memo :: Behavior a -> Behavior a
memo source = unsafePerformIO $ do
    ref <- newIORef Nothing
    weakRef <- mkWeakPtr ref Nothing
    (thisInvalidated, invalidate) <- newEvent
    register (invalidated source) $ do
        ref <- deRefWeak weakRef
        case ref of
            Just ref -> return $ Just $ \callAfter -> do
                val <- readIORef ref
                case val of
                    Just _ -> do
                        writeIORef ref Nothing
                        invalidate callAfter
                    Nothing -> return () -- No change
            Nothing -> return Nothing
    return Behavior {
        value = do
            val <- readIORef ref -- TODO: possible race condition
            case val of
                Just val -> return val
                Nothing -> do
                    val <- value source
                    writeIORef ref $ Just val
                    return val,
        invalidated = thisInvalidated }
