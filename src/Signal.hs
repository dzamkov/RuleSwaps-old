{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Signal (
    Signal,
    defer,
    ReactT,
    runReactT,
    React,
    runReact,
    MonadReact (..)
) where

import Prelude hiding (map)
import Stride
import Data.IORef
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad (when, unless, forM_)
import Control.Applicative hiding (Const)

-- | Identifies a moment in time between signal updates. This is not based on
-- real time, but is ordered by occurence.
type Time = Int

-- | Describes the state of an input.
data InputState a = InputState {

    -- | Contains the values of this input at various points in time. Input
    -- data is organized such that the entry for any time is the stride from
    -- the previous entry's time. The 'start' of the first entry's stride is
    -- undefined.
    inputData :: Map Time (Stride a),

    -- | The signals that this input needs to inform of a change.
    inputProps :: [SignalProp] }

-- | Describes the state of a signal.
data SignalState a = SignalState {

    -- | Contains the values for the signal at various points in time. Each
    -- entry is a stride from the given time to the current time.
    signalData :: Map Time (Stride a),

    -- | Indicates wether propogations to this signal have been set up.
    signalReady :: Bool,

    -- | The signals that this signal needs to inform of a change.
    signalProps :: [SignalProp] }

-- | Contains the information needed to propogate a change through a signal
-- graph. No information about the change is given, just that there is one.
data SignalProp = forall a. SignalProp (IORef (SignalState a))
instance Eq SignalProp where
    (==) (SignalProp x) (SignalProp y) = x == unsafeCoerce y

-- | Propogates changes.
prop :: SignalProp -> IO ()
prop (SignalProp ref) = do
    state <- readIORef ref
    let dat = signalData state
    unless (Map.null dat) $ do
        writeIORef ref state { signalData = Map.empty }
        forM_ (signalProps state) prop

-- | Describes the static information about a signal which has a consistent
-- set of dependencies
data SignalInfo a = SignalInfo {

    -- | The state for this signal, also identifies the signal.
    signalState :: IORef (SignalState a),

    -- | The definition for this signal.
    signalDef :: forall f. (Applicative f)
        => Evaluator PrimSignal f -> f (Stride a) }

-- | The primitive definition of 'Signal', without the annoying @r@ parameter.
data PrimSignal a where
    Input :: IORef (InputState a) -> PrimSignal a
    Signal :: SignalInfo a -> PrimSignal a
    Defer :: IORef (SignalState a) -> PrimSignal (Signal r a) -> PrimSignal a

-- | Sets up or removes a propogation between the given signal and the signal
-- for the given state.
depend :: Bool -> PrimSignal a -> IORef (SignalState b) -> IO ()
depend v from to = res where
    prop = SignalProp to
    act True props = prop : props
    act False props = List.filter (/= prop) props
    res = case from of
        Input input -> modifyIORef input $ \x ->
            x { inputProps = act v $ inputProps x }
        Signal info -> modifyIORef (signalState info) $ \x ->
            x { signalProps = act v $ signalProps x }
        Defer ref _ -> modifyIORef ref $ \x ->
            x { signalProps = act v $ signalProps x }

-- | Specifies a time before which all signal data is deleted.
type Window = Time

-- | Gets an evaluator for primitive signals for the given interval of time.
evalPrim :: Window -> Time -> Time -> Evaluator PrimSignal IO
evalPrim win time cur (sig :: PrimSignal a) = res where
    inputClean :: forall b. Map Time b -> Map Time b
    inputClean dat = case Map.lookupLT win dat of
        Just (s, _) -> snd $ Map.split (s - 1) dat
        Nothing -> dat
    cacheClean :: forall b. Map Time b -> Map Time b
    cacheClean = snd . Map.split (win - 1)
    cacheCheck subeval dat = do
        (stride, nDat') <- case Map.lookup time dat of
            Nothing -> do
                stride <- subeval
                return (stride, Map.insert time stride dat)
            Just stride -> return (stride, dat)
        let nDat = Map.insert cur (stay $ end stride) nDat'
        return (stride, nDat)
    stateCheck subeval ref = do
        state <- readIORef ref
        let dat = cacheClean $ signalData state
        let ready = signalReady state
        (stride, nDat) <- cacheCheck (subeval ready) dat
        writeIORef ref state { signalData = nDat, signalReady = True }
        return stride
    res = case sig of
        (Input ref) -> do
            state <- readIORef ref
            let dat = inputClean $ inputData state
            writeIORef ref state { inputData = dat }
            let (Just (s, first)) = Map.lookupLE time dat
            let (_, after) = Map.split s dat
            let strides = List.map snd $ Map.toAscList after
            return $ if List.null strides
                then stay $ end first
                else List.foldl1 glue strides
        (Signal info) -> do
            let ref = signalState info
            let subeval ready = signalDef info (\sig -> do
                  unless ready $ depend True sig ref
                  evalPrim win time cur sig)
            stateCheck subeval ref
        (Defer ref source) -> do
            let subeval ready = do
                unless ready $ depend True source ref
                sourceStride <- evalPrim win time cur source
                case sourceStride of
                    Stay target -> eval (\prim -> do
                        unless ready $ depend True prim ref
                        evalPrim win time cur prim) target
                    Stride oTarget nTarget -> do
                        oStride <- eval (\prim -> do
                            when ready $ depend False prim ref
                            evalPrim win time cur prim) oTarget
                        nStride <- eval (\prim -> do
                            depend True prim ref
                            evalPrim win cur cur prim) nTarget
                        return $ stride (start oStride) (end nStride)
                    Complex _ -> undefined
            stateCheck subeval ref

-- | Identifies a value of type @a@ that can vary within the context of a
-- 'ReactT' procedure.
data Signal r a where
    Const :: a -> Signal r a
    Prim :: PrimSignal a -> Signal r a

instance Functor (Signal r) where
    fmap f other = deltor (\eval -> (f <$>) <$> eval other)
instance Applicative (Signal r) where
    pure = Const
    (<*>) x y = deltor (\eval -> (<*>) <$> eval x <*> eval y)
instance Deltor (Signal r) where
    deltor = signal

-- | Constructs a signal from the given defining function.
{-# ANN signal "Hlint: Avoid lambda Found" #-}
signal :: (forall g. (Applicative g)
    => Evaluator (Signal r) g -> g (Stride a)) -> Signal r a
signal def = res where
    evalConst (Const x) = Just (Stay x)
    evalConst _ = Nothing
    info = unsafePerformIO $ do
        state <- newIORef SignalState {
            signalData = Map.empty,
            signalReady = False,
            signalProps = [] }
        return SignalInfo {
            signalState = state,
            signalDef = \evalPrim -> def $ eval evalPrim }
    res = case def evalConst of
        Just x -> Const $ start x
        Nothing -> Prim $ Signal info

-- | Gets an evaluator for a signal, given an evaluator for a 'PrimSignal'.
eval :: (Applicative f) => Evaluator PrimSignal f -> Evaluator (Signal r) f
eval _ (Const x) = pure $ stay x
eval evalPrim (Prim prim) = evalPrim prim

-- | Creates a signal that reads from the signal chosen by another signal.
defer :: Signal r (Signal r a) -> Signal r a
defer (Const x) = x
defer (Prim prim) = unsafePerformIO $ do
    ref <- newIORef SignalState {
        signalData = Map.empty,
        signalReady = False,
        signalProps = [] }
    return $ Prim $ Defer ref prim

-- | Describes an output.
type Output = IORef Time

-- TODO: Multithreading

-- | A procedure which can manipulate signals in the context @r@.
newtype ReactT r m a = ReactT (StateT (Time, [Output]) m a)
    deriving (Functor, Applicative, Monad, MonadTrans)

-- | Runs a 'ReactT'.
runReactT :: forall m a. (Monad m) => (forall r. ReactT r m a) -> m a
runReactT (ReactT s) = evalStateT s (0, [])

-- | A procedure which can manipulate signals.
type React r = ReactT r Identity

-- | Runs a 'React'.
runReact :: (forall r. React r a) -> a
runReact x = runIdentity $ runReactT x

-- | @m@ is a monad similar to 'React' which can manipulate signals in the
-- context @r@.
class (Monad m, Monad n) => MonadReact r m n | m -> r n where

    -- | Creates a new input signal, returning the signal and a procedure which
    -- can be used to later change its value.
    input :: a -> m (Signal r a, Delta a -> n a)

    -- | Creates a new output for a signal, returning both current value and a
    -- procedure which can be used to get stride since the last time the output
    -- was accessed.
    output :: Signal r a -> m (a, n (Stride a))

instance Monad m => MonadReact r (ReactT r m) (ReactT r m) where
    input cur = ReactT $ do
        (time, _) <- get
        let ref = unsafePerformIO $ newIORef InputState {
              inputData = Map.singleton time (stay cur),
              inputProps = [] }
        let update delta = ReactT $ do
            (time, outputs) <- get
            let nTime = time + 1
            put (nTime, outputs)
            unsafePerformIO $ do
                state <- readIORef ref
                let dat = inputData state
                let (_, last) = Map.findMax dat
                let new = delta $ end last
                let nState = state { inputData = Map.insert nTime new dat }
                writeIORef ref nState
                forM_ (inputProps state) prop
                return $ return $ end last
        return (Prim $ Input ref, update)
    output sig = ReactT $ do
        (time, outputs) <- get
        let window outputs = minimum <$> mapM readIORef outputs
        let (ref, cur, nOutputs) = unsafePerformIO $ do
            ref <- newIORef time
            let nOutputs = ref : outputs
            curWindow <- window nOutputs
            last <- eval (evalPrim curWindow time time) sig
            return (ref, end last, nOutputs)
        put (time, nOutputs)
        let look = ReactT $ do
            (time, outputs) <- get
            unsafePerformIO $ do
                from <- readIORef ref
                curWindow <- window outputs
                last <- eval (evalPrim curWindow from time) sig
                writeIORef ref time
                return $ return last
        return (cur, look)
