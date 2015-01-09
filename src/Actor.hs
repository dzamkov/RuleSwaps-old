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
    Source,
    noSource,
    source,
    mapSource,
    (<>),
    ActorT,
    Actor,
    liftActor,
    MonadActor (..),
    stepper,
    blocking,
    IOContext,
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
import Control.Monad (void, when)
import Control.Applicative

-- | A communication channel for an 'ActorT'.
type family Channel r :: * -> *

-- | An source triggered by a single channel.
data ChannelSource r a = forall b. ChannelSource (Channel r b) (b -> a)

-- | A set of receive-only channels, each mapped to produce a consistent type.
-- When waiting on an source, a single message from one of the channels will be
-- taken and returned.
newtype Source r a = Source [ChannelSource r a] deriving (Monoid)
instance Functor (Source r) where
    fmap = mapSource

-- | A source that is never triggered.
noSource :: Source r a
noSource = Source []

-- | Constructs an source for a channel. The source will fire when a message is
-- received, and the message will be removed from the channel.
source :: Channel r a -> Source r a
source channel = Source [ChannelSource channel id]

-- | Maps the result of a source.
mapSource :: (a -> b) -> Source r a -> Source r b
mapSource f (Source inputs) = Source $
    map (\(ChannelSource i g) -> ChannelSource i (f . g)) inputs

-- | A primitive instruction for an actor.
data ActorInstr r m a where

    -- | Creates a new channel.
    Spawn :: ActorInstr r m (Channel r a)

    -- | Sends a message on a channel, and waits for it to be received.
    Yield :: Channel r a -> a -> ActorInstr r m ()

    -- | Awaits a source.
    Await :: Source r a -> ActorInstr r m a

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

-- | Adds a base monad to an actor.
liftActor :: (Monad m) => Actor r a -> ActorT r m a
liftActor (ActorT program) = interpretWithMonad (\instr -> case instr of
    Spawn -> spawn
    Yield channel msg -> yield channel msg
    Await source -> await source
    Fork inner -> fork_ (liftActor inner)) program

-- | @m@ is a context in which communication on channels in the context of @r@
-- is possible.
class (Functor m, Monad m) => MonadActor r m | m -> r where

    -- | Creates a new communication channel.
    spawn :: m (Channel r a)

    -- | Sends a message on a channel.
    send :: Channel r a -> a -> m ()
    send channel msg = fork_ $ yield channel msg

    -- | Sends a message on a channel and waits for it to be received.
    yield :: Channel r a -> a -> m ()

    -- | Awaits asource, which is composed of many channels. Execution
    -- will not continue until a message is received on one of the channels.
    await :: Source r a -> m a

    -- | Forks an actor, ignoring the result of the child actor.
    fork_ :: m () -> m ()
    fork_ = void . fork

    -- | Forks an actor, creating a source for when the child actor terminates.
    fork :: m a -> m (Source r a)
    fork child = do
        doneChan <- spawn
        fork_ $ child >>= yield doneChan
        return $ source doneChan

instance Monad m => MonadActor r (ActorT r m) where
    spawn = ActorT $ singleton Spawn
    yield output msg = ActorT $ singleton $ Yield output msg
    await = ActorT . singleton . Await
    fork_ = ActorT . singleton . Fork

-- | Constructs an actor which stores a value that is updated by the given
-- source.
stepper :: (MonadActor r m) => a -> Source r a -> m (m a)
stepper initial e = do
    queryChan <- spawn
    let listen cur = do
        msg <- await $ (Left <$> source queryChan) <> (Right <$> e)
        case msg of
            Left respond -> do
                respond cur
                listen cur
            Right new -> listen new
    fork_ $ listen initial
    return $ do
        responseChan <- spawn
        yield queryChan $ yield responseChan
        await $ source responseChan

-- | A possible context for an actor, indicating the use of 'IO' channels.
-- See 'startActorIO'.
data IOContext

-- | Describes an unread message in an 'IOChannel'. The associated procedure
-- is the continuation of the 'yield' that put the message there.
type IOBacklog a = (a, IO ())

-- | Describes a handler waiting on an 'IOChannel'. The first procedure
-- determines whether the handler is still valid, while the second is the
-- continuation of the 'await' that put the message there.
type IOHandler a = (IO Bool, a -> IO ())

-- | A channel for communication within an 'IO' context. The channel may either
-- have a backlog of unread messages, or a set of handlers ready to read
-- messages. Some handlers may not be willing to accept the message, returning
-- false.
newtype IOChannel a = IOChannel (IORef (Either [IOBacklog a] [IOHandler a]))
type instance Channel IOContext = IOChannel

-- | Creates a new channel.
newChannel :: IO (IOChannel a)
newChannel = IOChannel <$> newIORef (Right [])

-- | Sends a message on a channel. The continuation is provided.
yieldIO :: IOChannel a -> a -> ActorT IOContext IO b -> IO ()
yieldIO channel@(IOChannel ref) msg cont = do
    handler <- atomicModifyIORef ref (\state -> case state of
        Left msgs -> (Left ((msg, startActorIO cont) : msgs), Nothing)
        Right [] -> (Left [(msg, startActorIO cont)], Nothing)
        Right (head : tail) -> (Right tail, Just head))
    case handler of
        Nothing -> return ()
        Just (checkValid, handler) -> do
            valid <- checkValid
            if valid
                then handler msg >> startActorIO cont
                else yieldIO channel msg cont

-- | Awaits a source in an 'IO' context. A continuation is provided.
awaitIO :: Source IOContext a -> (a -> ActorT IOContext IO b) -> IO ()
awaitIO (Source inputs) cont = res where
    processInput checkValid (ChannelSource (IOChannel ref) f) =
        let newHandlers = [(checkValid, startActorIO . cont . f)]
        in atomicModifyIORef ref (\state -> case state of
            Left ((msg, go) : tail) -> (Left tail, Just (f msg, go))
            Left [] -> (Right newHandlers, Nothing)
            Right handlers -> (Right (handlers ++ newHandlers), Nothing))
    res = case inputs of
        [] -> return ()
        [single] -> do
            value <- processInput (return True) single
            case value of
                Nothing -> return ()
                Just (msg, go) -> go >> startActorIO (cont msg)
        inputs -> do
            preHandled <- newIORef False
            let checkValid = atomicModifyIORef preHandled
                  (\preHandled -> (True, not preHandled))
            let setup [] = return ()
                setup (input : inputs) = do
                    value <- processInput checkValid input
                    case value of
                        Nothing -> setup inputs
                        Just (msg, go) -> do
                            valid <- checkValid
                            when valid (go >> startActorIO (cont msg))
            setup inputs

-- | Marks an actor as having potentially blocking 'IO' effects. The resulting
-- actor will be properly non-blocking when used with 'startActorIO'.
blocking :: ActorT IOContext IO a -> ActorT IOContext IO a
blocking inner = do
    resChan <- spawn
    liftIO $ forkIO $ startActorIO $ do
        res <- inner
        yield resChan res
    await (source resChan)

-- | Runs an 'Actor' in the context of 'IO'. Actors and channels set up in
-- this way may all communicate with each other.
runActorIO :: ActorT IOContext IO a -> IO a
runActorIO actor = do
    var <- newEmptyMVar
    startActorIO (actor >>= liftIO . putMVar var)
    takeMVar var

-- | Non-blocking version of 'runActorIO', provided that all potentially
-- blocking 'IO' effects are marked using 'blocking'.
startActorIO :: ActorT IOContext IO a -> IO ()
startActorIO (ActorT (viewT -> next)) = next >>= process where
    process :: ProgramViewT (ActorInstr IOContext IO) IO a -> IO ()
    process (Return _) = return ()
    process (Spawn :>>= cont) = do
        channel <- newChannel
        startActorIO $ ActorT $ cont channel
    process (Yield channel msg :>>= cont) =
        yieldIO channel msg (ActorT $ cont ())
    process (Await source :>>= cont) =
        awaitIO source (ActorT . cont)
    process (Fork child :>>= cont) = do
        startActorIO $ ActorT $ cont ()
        startActorIO child
