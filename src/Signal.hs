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
    defer,
    signal,
    dmap,
    plex2,
    plex3,
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
        => (forall r b. (HasDelta b) => Signal r b -> f (b, Delta b))
        -> f (a, Delta a) }

-- | Identifies a value of type @a@ that can vary within the context of a
-- 'ReactT' procedure.
data Signal r a where
    Const :: a -> Signal r a
    Input :: IORef (InputState a) -> Signal r a
    Signal :: SignalInfo a -> Signal r a
    Defer :: IORef (SignalState a) -> Signal r (Signal r a) -> Signal r a
type instance Delta (Signal r a) = SimpDelta (Signal r a)

-- | Specifies a time before which all signal data is deleted.
type Window = Time

-- | Sets up or removes a propogation between the given signal and the signal
-- for the given state.
depend :: Bool -> Signal r a -> IORef (SignalState b) -> IO ()
depend v from to = res where
    prop = SignalProp to
    act True props = prop : props
    act False props = List.filter (/= prop) props
    res = case from of
        Const _ -> return ()
        Input input -> modifyIORef input $ \x ->
            x { inputProps = act v $ inputProps x }
        Signal info -> modifyIORef (signalState info) $ \x ->
            x { signalProps = act v $ signalProps x }
        Defer ref _ -> modifyIORef ref $ \x ->
            x { signalProps = act v $ signalProps x }

-- | Gets the value of the given signal at the given time, along with the delta
-- since then.
eval :: forall r a. (HasDelta a) => Window -> Time -> Time
    -> Signal r a -> IO (a, Delta a)
eval win time cur sig = res where
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
        (Const x) -> return (x, keep)
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
                  eval win time cur sig)
            stateCheck subeval ref
        (Defer ref source) -> do
            let subeval ready = do
                unless ready $ depend True source ref
                (target, sourceDelta) <- eval win time cur source
                case sourceDelta of
                    Keep -> do
                        unless ready $ depend True target ref
                        eval win time cur target
                    Set nTarget -> do
                        when ready $ depend False target ref
                        depend True nTarget ref
                        (oValue, _) <- eval win time cur target
                        (nValue, _) <- eval win cur cur nTarget
                        return (oValue, set nValue)
            stateCheck subeval ref

-- | Constructs a signal from a transformation of existing signals. This
-- assumes that the dependencies of the signal will never change.
{-# ANN signal "HLint: ignore Avoid lambda" #-}
signal :: (forall f. (Applicative f)
    => (forall b. (HasDelta b) => Signal r b -> f (b, Delta b))
    -> f (a, Delta a))
    -> Signal r a
signal f = res where
    -- Check if the signal is a constant.
    quickEval (Const x) = Just (x, keep)
    quickEval _ = Nothing
    res = case f quickEval of
        Just (x, _) -> Const x
        Nothing -> normal

    -- Create a dependency between signalState and f to prevent sharing of
    -- states for different signals.
    normal = Signal $ unsafePerformIO $ do
        state <- newIORef SignalState {
            signalData = Map.empty,
            signalReady = False,
            signalProps = [] }
        return SignalInfo {
            signalState = state,
            signalEval = \eval -> f eval }

-- | Applies a 'DeltaMap' to a signal.
dmap :: (HasDelta a) => DeltaMap a b -> Signal r a -> Signal r b
dmap (f, df) i = signal $ \eval ->
    (\(oi, di) -> (f oi, df oi di)) <$> eval i

-- | Creates a signal that reads from the signal chosen by another signal.
defer :: (HasDelta a) => Signal r (Signal r a) -> Signal r a
defer (Const x) = x
defer source = unsafePerformIO $ do
    ref <- newIORef SignalState {
        signalData = Map.empty,
        signalReady = False,
        signalProps = [] }
    return $ Defer ref source

-- | Constructs a signal by multiplexing the given signals.
plex2 :: (HasDelta a, HasDelta b)
    => Signal r a -> Signal r b -> Signal r (a, b)
plex2 x y = signal $ \eval ->
    (\(ox, dx) (oy, dy) -> ((ox, oy), (dx, dy))) <$>
    eval x <*> eval y

-- | Constructs a signal by multiplexing the given signals.
plex3 :: (HasDelta a, HasDelta b, HasDelta c)
    => Signal r a -> Signal r b -> Signal r c -> Signal r (a, b, c)
plex3 x y z = signal $ \eval ->
    (\(ox, dx) (oy, dy) (oz, dz) -> ((ox, oy, oz), (dx, dy, dz))) <$>
    eval x <*> eval y <*> eval z

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
    return (Input ref, update)

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
        (cur, _) <- eval curWindow time time sig
        return (ref, cur, nOutputs)
    put (time, nOutputs)
    let look = ReactT $ do
        (time, outputs) <- get
        unsafePerformIO $ do
            from <- readIORef ref
            curWindow <- window outputs
            (last, delta) <- eval curWindow from time sig
            writeIORef ref time
            return $ return (delta, apply delta last)
    return (cur, look)
