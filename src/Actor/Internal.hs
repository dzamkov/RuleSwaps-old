{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
module Actor.Internal where

import Data.Monoid
import Control.Monad.Identity (Identity)
import Control.Monad.Operational
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad (void)
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

    -- | Sends a message on a channel.
    Send :: Channel r a -> a -> ActorInstr r m ()

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

-- | @m@ is a context in which communication on channels in the context of @r@
-- is possible.
class (Functor m, Monad m) => MonadActor r m | m -> r where

    -- | Creates a new communication channel.
    spawn :: m (Channel r a)

    -- | Sends a message on a channel.
    send :: Channel r a -> a -> m ()

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
        fork_ $ child >>= send doneChan
        return $ source doneChan

instance Monad m => MonadActor r (ActorT r m) where
    spawn = ActorT $ singleton Spawn
    send output msg = ActorT $ singleton $ Send output msg
    await = ActorT . singleton . Await
    fork_ = ActorT . singleton . Fork

-- | Stops the current actor such that no further instructions are executed.
stop :: (MonadActor r m) => m a
stop = await noSource

-- | Adds a base monad to an actor.
liftActor :: (MonadActor r m) => Actor r a -> m a
liftActor (ActorT program) = interpretWithMonad (\instr -> case instr of
    Spawn -> spawn
    Send channel msg -> send channel msg
    Await source -> await source
    Fork inner -> fork_ (liftActor inner)) program

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
        send queryChan $ send responseChan
        await $ source responseChan
