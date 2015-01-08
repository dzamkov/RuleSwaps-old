{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}
module Actor (
    Channel,
    Event,
    mapEvent,
    (<>),
    ActorT,
    Actor,
    MonadActor (..),
    QIO,
    isLong,
    runQIO,
    toQIO,
    runActorIO,
    startActorIO
) where

import Data.Monoid
import Data.IORef
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad.Identity (Identity)
import Control.Monad.Operational
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad (void, when, unless)
import Control.Applicative

-- | A communication channel for an 'ActorT'.
type family Channel r :: * -> *

-- | An event triggered by a single channel.
data ChannelEvent r a = forall b. ChannelEvent (Channel r b) (b -> a)

-- | A set of receive-only channels, each mapped to produce a consistent type.
-- When waiting on an event, a single message from one of the channels will be
-- taken and returned.
newtype Event r a = Event [ChannelEvent r a] deriving (Monoid)
instance Functor (Event r) where
    fmap = mapEvent

-- | Constructs an event for a channel. The event will fire when a message is
-- received, and the message will be removed from the channel.
event :: Channel r a -> Event r a
event channel = Event [ChannelEvent channel id]

-- | Maps the result of an event.
mapEvent :: (a -> b) -> Event r a -> Event r b
mapEvent f (Event inputs) = Event $
    map (\(ChannelEvent i g) -> ChannelEvent i (f . g)) inputs

-- | A primitive instruction for an actor.
data ActorInstr r m a where

    -- | Creates a new channel.
    Spawn :: ActorInstr r m (Channel r a)

    -- | Sends a message on a channel.
    Send :: Channel r a -> a -> ActorInstr r m ()

    -- | Awaits an event.
    Await :: Event r a -> ActorInstr r m a

    -- | Forks the actor.
    Fork :: ActorT r m () -> ActorInstr r m ()

-- | Describes a procedure which can communicate on channels within the context
-- of @r@, and execute effects within the context of @m@. When finished, the
-- actor returns a value of type @a@.
newtype ActorT r m a = ActorT (ProgramT (ActorInstr r m) m a)
    deriving (Functor, Applicative, Monad)
instance MonadTrans (ActorT r) where
    lift = ActorT . lift
instance MonadIO m => MonadIO (ActorT r m) where
    liftIO = lift . liftIO

-- | Describes a procedure which can communicate on channels within the context
-- of @r@. When finished, the actor returns a value of type @a@.
type Actor r = ActorT r Identity

-- | @m@ is a context in which communication on channels in the context of @r@
-- is possible.
class Monad m => MonadActor r m | m -> r where

    -- | Creates a new communication channel.
    spawn :: m (Channel r a)

    -- | Sends a message on a channel.
    send :: Channel r a -> a -> m ()

    -- | Awaits an event, which is composed of many channels. Execution
    -- will not continue until a message is received on one of the channels.
    await :: Event r a -> m a

    -- | Forks an actor, creating an event for when the child actor terminates.
    fork :: m a -> m (Event r a)

instance Monad m => MonadActor r (ActorT r m) where
    spawn = ActorT $ singleton Spawn
    send output msg = ActorT $ singleton $ Send output msg
    await = ActorT . singleton . Await
    fork child = do
        done <- spawn
        ActorT $ singleton $ Fork (child >>= send done)
        return (event done)

-- | Similar to 'IO', but tracks whether procedures are long or blocking.
data QIO a = QIO Bool (IO a)
instance Functor QIO where
    fmap f (QIO long io) = QIO long (fmap f io)
instance Applicative QIO where
    pure = QIO False . pure
    (<*>) (QIO aLong a) (QIO bLong b) = QIO (aLong || bLong) (a <*> b)
instance Monad QIO where
    return = pure
    (>>=) (QIO _ a) f = QIO True (a >>= (runQIO . f))
instance MonadIO QIO where
    liftIO = QIO True

-- | Indicates whether the given 'QIO' procedure is long or blocking.
isLong :: QIO a -> Bool
isLong (QIO long _) = long

-- | Runs a 'QIO' procedure in the context of an 'IO'.
runQIO :: QIO a -> IO a
runQIO (QIO _ io) = io

-- | Converts an 'IO' into a 'QIO' procedure, given whether it is long.
toQIO :: Bool -> IO a -> QIO a
toQIO = QIO

-- | A possible context for an actor, indicating the use of 'IO' channels.
-- See 'startActorIO'.
data IOContext

-- | A channel for communication within an 'IO' context. The channel may either
-- have a backlog of unread messages, or a set of handlers ready to read
-- messages. Some handlers may not be willing to accept the message, returning
-- false.
newtype IOChannel a = IOChannel (IORef (Either (a, [a]) [a -> IO Bool]))
type instance Channel IOContext = IOChannel

-- | Creates a new channel.
newChannel :: IO (IOChannel a)
newChannel = IOChannel <$> newIORef (Right [])

-- | Sends a message on a channel. This is non-blocking.
sendChannel :: IOChannel a -> a -> IO ()
sendChannel channel@(IOChannel ref) msg = do
    handler <- atomicModifyIORef ref (\state -> case state of
        Left (head, tail) -> (Left (msg, head : tail), Nothing)
        Right [] -> (Left (msg, []), Nothing)
        Right (head : tail) -> (Right tail, Just head))
    case handler of
        Nothing -> return ()
        Just handler -> do
            handled <- handler msg
            unless handled $ sendChannel channel msg

-- | Awaits an event in an 'IO' context. A continuation is provided, along
-- with a parameter indicating if this procedure may block.
awaitIO :: Event IOContext a -> Bool -> (a -> ActorT IOContext QIO b) -> IO ()
awaitIO (Event inputs) blocking cont = res where
    processInput handle (ChannelEvent (IOChannel ref) f) =
        atomicModifyIORef ref (\state -> case state of
            Left (head, tail) ->
                let nState = case tail of
                      head : tail -> Left (head, tail)
                      [] -> Right []
                in (nState, Just (f head))
            Right handlers ->
                let nHandlers = handlers ++ [handle . f]
                in (Right nHandlers, Nothing))
    res = case inputs of
        [] -> return ()
        [single] ->
            let handle msg = runActorIO' False (cont msg) >> return True
            in do
                value <- processInput handle single
                case value of
                    Nothing -> return ()
                    Just msg -> runActorIO' blocking $ cont msg
        inputs -> do
            preHandled <- newIORef False
            let handle blocking msg = do
                handled <- atomicModifyIORef preHandled
                    (\preHandled -> (True, not preHandled))
                when handled $ runActorIO' blocking $ cont msg
                return handled
            let setup [] = return ()
                setup (input : inputs) = do
                    value <- processInput (handle False) input
                    case value of
                        Nothing -> setup inputs
                        Just msg -> do
                            handle blocking msg
                            return ()
            setup inputs

-- | Like 'runActorIO', but has a parameter to decide whether it is blocking.
runActorIO' :: Bool -> ActorT IOContext QIO a -> IO ()
runActorIO' blocking (ActorT (viewT -> next)) = res where
    process :: Bool -> ProgramViewT (ActorInstr IOContext QIO) QIO a -> IO ()
    process _ (Return _) = return ()
    process blocking (Spawn :>>= cont) = do
        channel <- newChannel
        runActorIO' blocking $ ActorT $ cont channel
    process blocking (Send output msg :>>= cont) = do
        sendChannel output msg
        runActorIO' blocking $ ActorT $ cont ()
    process blocking (Await event :>>= cont) =
        awaitIO event blocking (ActorT . cont)
    process blocking (Fork child :>>= cont) = do
        runActorIO' False child
        runActorIO' blocking $ ActorT $ cont ()
    res = case (blocking, isLong next) of
        (False, True) -> void $ forkIO (runQIO next >>= process True)
        (blocking, _) -> runQIO next >>= process blocking

-- | Runs an 'Actor' in the context of 'IO' using lightweight
-- threads. Actors and channels set up in this way may all communicate with
-- each other.
runActorIO :: ActorT IOContext QIO a -> IO a
runActorIO actor = do
    var <- newEmptyMVar
    runActorIO' True (actor >>= liftIO . putMVar var)
    takeMVar var

-- | Non-blocking version of 'runActorIO'.
startActorIO :: ActorT IOContext QIO a -> IO ()
startActorIO = runActorIO' False
