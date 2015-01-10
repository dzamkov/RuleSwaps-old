{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ViewPatterns #-}
module Actor.IO (
    awaitBlocking,
    blocking,
    IOContext,
    runActorIO,
    startActorIO
) where

import Actor.Internal
import Data.IORef
import Data.Foldable (forM_)
import Control.Monad.Operational
import Control.Monad.IO.Class
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Applicative

-- | A possible context for an actor, indicating the use of 'IO' channels.
-- See 'startActorIO'.
data IOContext

-- | Describes a handler waiting on an 'IOChannel'. The first procedure
-- determines whether the handler is still valid, while the second is the
-- continuation of the 'await' that put the message there.
type IOHandler a = (IO Bool, a -> IO ())

-- | A channel for communication within an 'IO' context. The channel may either
-- have a backlog of unread messages, or a set of handlers ready to read
-- messages. Some handlers may not be willing to accept the message, returning
-- false.
newtype IOChannel a = IOChannel (IORef (Either [a] [IOHandler a]))
type instance Channel IOContext = IOChannel

-- | Creates a new channel.
newChannel :: IO (IOChannel a)
newChannel = IOChannel <$> newIORef (Right [])

-- | Sends a message on a channel. This may return a continuation that should
-- be fufilled when the current procedure is complete.
sendIO :: IOChannel a -> a -> IO (Maybe (IO ()))
sendIO channel@(IOChannel ref) msg = do
    handler <- atomicModifyIORef ref (\state -> case state of
        Left msgs -> (Left (msgs ++ [msg]), Nothing)
        Right [] -> (Left [msg], Nothing)
        Right (head : tail) -> (Right tail, Just head))
    case handler of
        Nothing -> return Nothing
        Just (checkValid, handler) -> do
            valid <- checkValid
            if valid
                then return $ Just $ handler msg
                else sendIO channel msg

-- | Given a "handled" variable. Determines whether the current thread should
-- handle whatever it is, and updates the variable accordingly.
shouldHandle :: IORef Bool -> IO Bool
shouldHandle ref = atomicModifyIORef ref (\handled -> (True, not handled))

-- | Awaits a source in an 'IO' context. A continuation is provided. An
-- optional "handled" variable may be provided.
awaitIO :: Source IOContext a -> Maybe (IORef Bool) -> (a -> IO ()) -> IO ()
awaitIO (Source inputs) handled cont = res where
    processInput checkValid (ChannelSource (IOChannel ref) f) =
        let newHandlers = [(checkValid, cont . f)]
        in atomicModifyIORef ref (\state -> case state of
            Left (msg : tail) -> (Left tail, Just (f msg))
            Left [] -> (Right newHandlers, Nothing)
            Right handlers -> (Right (handlers ++ newHandlers), Nothing))
    res = case inputs of
        [] -> return ()
        [single] -> case handled of
            Nothing -> do
                value <- processInput (return True) single
                forM_ value cont
            Just handled -> do
                value <- processInput (shouldHandle handled) single
                forM_ value $ \msg -> do
                    valid <- shouldHandle handled
                    when valid $ cont msg
        inputs -> do
            handled <- case handled of
                Nothing -> newIORef False
                Just handled -> return handled
            let setup [] = return ()
                setup (input : inputs) = do
                    value <- processInput (shouldHandle handled) input
                    case value of
                        Nothing -> setup inputs
                        Just msg -> do
                            valid <- shouldHandle handled
                            when valid $ cont msg
            setup inputs

-- | Lifts a blocking IO effect, allowing it to be interrupted by a source.
awaitBlocking :: IO a -> Source IOContext b -> ActorT IOContext IO (Either a b)
awaitBlocking inner int = do
    var <- liftIO newEmptyMVar
    liftIO $ do
        handled <- newIORef False
        thread <- liftIO $ forkIO $ do
            res <- inner
            valid <- shouldHandle handled
            when valid $ putMVar var $ Left res
        awaitIO int (Just handled) $ \msg -> do
            putMVar var $ Right msg
            killThread thread
    blocking $ takeMVar var

-- | Lifts a blocking IO effect.
blocking :: IO a -> ActorT IOContext IO a
blocking inner = do
    resChan <- spawn
    liftIO $ forkIO $ startActorIO $ do
        res <- liftIO inner
        send resChan res
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
    process (Send channel msg :>>= cont) = do
        contOther <- sendIO channel msg
        case contOther of
            Nothing -> startActorIO $ ActorT $ cont ()
            Just contOther -> do
                startActorIO $ ActorT $ cont ()
                contOther
    process (Await source :>>= cont) =
        awaitIO source Nothing (startActorIO . ActorT . cont)
    process (Fork child :>>= cont) = do
        startActorIO $ ActorT $ cont ()
        startActorIO child
