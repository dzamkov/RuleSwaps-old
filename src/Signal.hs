{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Signal (
    Signal,
    cache,
    defer,
    ReactT,
    runReactT,
    React,
    runReact,
    input,
    output,
) where

import Prelude hiding (map)
import Delta
import Data.IORef
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import "mtl" Control.Monad.Identity (Identity, runIdentity)
import "mtl" Control.Monad.State (StateT, evalStateT, get, put)
import "mtl" Control.Monad.Trans (MonadTrans (..))
import Control.Monad (when, unless, forM_)
import Control.Applicative hiding (Const)
import Control.Arrow (second)

-- | Identifies a moment in time between signal updates. This is not based on
-- real time, but is ordered by occurence.
type Time = Int

-- | Describes the state of an input.
data InputState a = InputState {

    -- | Contains the values of this input at various points in time, along
    -- with the deltas immediately preceding them.
    inputData :: Map Time (Delta a, a),

    -- | The signals that this input needs to inform of a change.
    inputProps :: [SignalProp] }

-- | Describes the state of a signal.
data SignalState a = SignalState {

    -- | Contains the values for the signal at various points in time, along
    -- with the deltas since then, if they are known and valid.
    signalData :: Map Time (a, Maybe (Delta a)),

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
    let f cont item = case item of
          (value, Just _) -> (True, (value, Nothing))
          item -> (cont, item)
    let (cont, nDat) = Map.mapAccum f False dat
    writeIORef ref state { signalData = nDat }
    when cont $ forM_ (signalProps state) prop

-- | Describes the static information about a signal which has a consistent
-- set of dependencies
data SignalInfo a = SignalInfo {

    -- | The state for this signal, also identifies the signal.
    signalState :: IORef (SignalState a),

    -- | Evaluates the signal using a function which evaluates a dependent
    -- signal.
    signalEval :: forall f. (Applicative f)
        => (forall r b. (HasDelta b) => PrimSignal r b -> f (b, Delta b))
        -> f (a, Delta a) }

-- | Like 'Signal' but the type @a@ must have the constraint @HasDelta@,
-- making 'PrimSignal' unsuitable as an instance of 'Functor'.
data PrimSignal r a where
    Input :: IORef (InputState a) -> PrimSignal r a
    Signal :: SignalInfo a -> PrimSignal r a
    Defer :: IORef (SignalState a)
        -> PrimSignal r (Signal r a) -> PrimSignal r a

-- | Sets up or removes a propogation between the given signal and the signal
-- for the given state.
depend :: Bool -> PrimSignal r a -> IORef (SignalState b) -> IO ()
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

-- | Gets the value of the given primitive signal at the given time, along with
-- the delta since then.
evalPrim :: forall r a. (HasDelta a) => Window -> Time -> Time
    -> PrimSignal r a -> IO (a, Delta a)
evalPrim win time cur sig = res where
    inputClean :: forall b. Map Time b -> Map Time b
    inputClean dat = case Map.lookupLT win dat of
        Just (s, _) -> snd $ Map.split (s - 1) dat
        Nothing -> dat
    cacheClean :: forall b. Map Time b -> Map Time b
    cacheClean = snd . Map.split (win - 1)
    cacheCheck :: IO (a, Delta a) -> Map Time (a, Maybe (Delta a))
        -> IO (a, Delta a, Map Time (a, Maybe (Delta a)))
    cacheCheck subeval dat = do
        (value, delta, nDat') <- case Map.lookup time dat of
            Nothing -> do
                (value, delta) <- subeval
                return (value, delta, Map.insert time (value, Just delta) dat)
            Just (value, Nothing) -> do
                (_, delta) <- subeval
                return (value, delta, Map.insert time (value, Just delta) dat)
            Just (value, Just delta) -> return (value, delta, dat)
        let nDat = Map.insert cur (apply delta value, Just keep) nDat'
        return (value, delta, nDat)
    stateCheck subeval ref = do
        state <- readIORef ref
        let dat = cacheClean $ signalData state
        let ready = signalReady state
        (value, delta, nDat) <- cacheCheck (subeval ready) dat
        writeIORef ref state { signalData = nDat, signalReady = True }
        return (value, delta)
    res = case sig of
        (Input ref) -> do
            state <- readIORef ref
            let dat = inputClean $ inputData state
            writeIORef ref state { inputData = dat }
            let (Just (s, (_, value))) = Map.lookupLE time dat
            let (_, after) = Map.split s dat
            let events = Map.toAscList after
            let delta = List.foldl (\accum (_, (d, _)) ->
                  merge accum d) keep events
            return (value, delta)
        (Signal info) -> do
            let ref = signalState info
            let subeval ready = signalEval info (\sig -> do
                  unless ready $ depend True sig ref
                  evalPrim win time cur sig)
            stateCheck subeval ref
        (Defer ref source) -> do
            let subeval ready = do
                unless ready $ depend True source ref
                (target, sourceDelta) <- evalPrim win time cur source
                case sourceDelta of
                    Keep -> evalToDelta (\prim -> do
                        unless ready $ depend True prim ref
                        evalPrim win time cur prim) target
                    Set nTarget -> do
                        (oValue, _) <- eval (\prim -> do
                            when ready $ depend False prim ref
                            evalPrim win time cur prim) target
                        (nValue, _) <- eval (\prim -> do
                            depend True prim ref
                            evalPrim win cur cur prim) nTarget
                        return (oValue, set nValue)
            stateCheck subeval ref

-- | Identifies a value of type @a@ that can vary within the context of a
-- 'ReactT' procedure.
data Signal r a where
    Const :: a -> Signal r a
    Prim :: (HasDelta a) => PrimSignal r a -> Signal r a
    Ap :: Signal r (a -> b) -> Signal r a -> Signal r b

type instance Delta (Signal r a) = SimpDelta (Signal r a)
instance Functor (Signal r) where
    fmap f (Const x) = Const (f x)
    fmap f other = Ap (Const f) other
instance Applicative (Signal r) where
    pure = Const
    (<*>) (Const f) (Const x) = Const (f x)
    (<*>) x y = Ap x y

-- | Evaluates the given signal using a function which evaluates a given
-- primitive signal.
eval :: (Applicative f)
    => (forall r b. (HasDelta b) => PrimSignal r b -> f (b, Delta b))
    -> Signal r a -> f (a, PureDelta a)
eval _ (Const x) = pure (x, Simple Keep)
eval evalPrim (Prim prim) = second Complex <$> evalPrim prim
eval evalPrim (Ap left right) =
    (\(f, df) (x, dx) -> (f x, case (df, dx) of
        (Simple Keep, Simple Keep) -> Simple Keep
        (Simple Keep, Simple (Set nx)) -> Simple $ Set $ f nx
        (Simple Keep, Complex dx) | isKeep dx -> Simple Keep
        (Simple Keep, Complex dx) -> Simple $ Set $ f $ apply dx x
        (Simple (Set nf), dx) -> Simple $ Set $ nf $ apply dx x
        (Complex df, Simple Keep) -> Complex $ df x
        (Complex df, Simple (Set nx)) -> Simple $ Set $ apply df f nx
        (Complex df, Complex dx) | isKeep dx -> Complex $ df x
        (Complex df, Complex dx) -> Simple $ Set (apply df f $ apply dx x)
    )) <$> eval evalPrim left <*> eval evalPrim right

-- | Like 'eval', but requires @a@ to satisfy the constraint 'HasDelta', and
-- returns a 'Delta' instead of a 'PureDelta'.
evalToDelta :: (Applicative f, HasDelta a)
    => (forall r b. (HasDelta b) => PrimSignal r b -> f (b, Delta b))
    -> Signal r a -> f (a, Delta a)
evalToDelta evalPrim sig = second toDelta <$> eval evalPrim sig

-- | Specifies a time before which all signal data is deleted.
type Window = Time

-- | Converts a 'Signal' into a 'PrimSignal'.
primify :: (HasDelta a) => Signal r a -> PrimSignal r a
primify sig = Signal $ unsafePerformIO $ do
    state <- newIORef SignalState {
        signalData = Map.empty,
        signalReady = False,
        signalProps = [] }
    return SignalInfo {
        signalState = state,
        signalEval = (`evalToDelta` sig) }

-- | Insures the given signal will be only be evaluated once per update.
cache :: (HasDelta a) => Signal r a -> Signal r a
cache x@(Const _) = x
cache other = Prim $ primify other

-- | Creates a signal that reads from the signal chosen by another signal.
defer :: (HasDelta a) => Signal r (Signal r a) -> Signal r a
defer (Const x) = x
defer source = unsafePerformIO $ do
    ref <- newIORef SignalState {
        signalData = Map.empty,
        signalReady = False,
        signalProps = [] }
    return $ Prim $ Defer ref (primify source)

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

-- | Creates a new input signal, returning the signal and a procedure which
-- can be used to later change its value.
input :: (HasDelta a, Monad m) => a
    -> ReactT r m (Signal r a, Delta a -> ReactT r m a)
input cur = ReactT $ do
    (time, _) <- get
    let ref = unsafePerformIO $ newIORef InputState {
          inputData = Map.singleton time (set cur, cur),
          inputProps = [] }
    let update delta = ReactT $ do
        (time, outputs) <- get
        let nTime = time + 1
        put (nTime, outputs)
        unsafePerformIO $ do
            state <- readIORef ref
            let dat = inputData state
            let (_, (_, last)) = Map.findMax dat
            let new = apply delta last
            let nState = state { inputData =
                  Map.insert nTime (delta, new) dat }
            writeIORef ref nState
            forM_ (inputProps state) prop
            return $ return last
    return (Prim $ Input ref, update)

-- | Creates a new output for a signal, returning both current value and a
-- procedure which can be used to get deltas and new values. Deltas are since
-- the last time the output has been inspected.
output :: (HasDelta a, Monad m) => Signal r a
    -> ReactT r m (a, ReactT r m (Delta a, a))
output sig = ReactT $ do
    (time, outputs) <- get
    let window outputs = minimum <$> mapM readIORef outputs
    let (ref, cur, nOutputs) = unsafePerformIO $ do
        ref <- newIORef time
        let nOutputs = ref : outputs
        curWindow <- window nOutputs
        (cur, _) <- eval (evalPrim curWindow time time) sig
        return (ref, cur, nOutputs)
    put (time, nOutputs)
    let look = ReactT $ do
        (time, outputs) <- get
        unsafePerformIO $ do
            from <- readIORef ref
            curWindow <- window outputs
            (last, delta) <- eval (evalPrim curWindow from time) sig
            writeIORef ref time
            return $ return (toDelta delta, apply delta last)
    return (cur, look)
