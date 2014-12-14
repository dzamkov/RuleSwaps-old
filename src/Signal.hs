{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Signal (
    Signal,
    map,
    ReactT,
    runReactT,
    React,
    runReact,
    input,
    update,
    output,
    look
) where

import Prelude hiding (map)
import Delta
import Data.IORef
import System.IO.Unsafe
import Control.Monad (forM_)
import "mtl" Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (MonadTrans (..))
import Control.Applicative hiding (Const)

-- | Contains the static information for a signal.
data SignalInfo i a = SignalInfo {

    -- | The state for this signal.
    state :: IORef (SignalState a),

    -- | Gets the change in the output for this signal given the change in the
    -- input. Side-effects may include relinking children for 'join' signals.
    process :: Delta i -> IO (Delta a) }

-- | Contains the mutable information for a signal.
data SignalState a = SignalState {

    -- | The current value for the signal.
    value :: a,

    -- | The signals that depend on this signal (used for propogation).
    props :: [SignalProp a],

    -- | The outputs associated with this signal.
    outputs :: [IORef (a, Delta a)] }

-- | Describes a link from a signal to a dependent signal.
data SignalProp a = forall i b. (HasDelta b)
    => SignalProp (Delta a -> Delta i) (SignalInfo i b)

-- | Directly changes the value of a signal and propogates changes.
change :: (HasDelta a) => IORef (SignalState a) -> Delta a -> IO ()
change _ delta | isKeep delta = return ()
change ref delta = do
    stateVal <- readIORef ref
    forM_ (outputs stateVal) $ \oRef ->
        modifyIORef oRef (\(c, d) -> (c, merge d delta))
    forM_ (props stateVal) $ \(SignalProp proj pInfo) -> do
        pDelta <- process pInfo (proj delta)
        change (state pInfo) pDelta
    writeIORef ref $ stateVal { value = apply delta $ value stateVal }

-- | Identifies a value of type @a@ that can vary within a context of type @r@.
data Signal r a where
    Const :: a -> Signal r a
    Signal :: SignalInfo i a -> Signal r a

-- | Constructs a impure signal based on the given linking function, input
-- gathering function, and mapping from input to output.
make :: forall i a. (HasDelta i, HasDelta a)
    => ((forall j b. SignalInfo j b -> (Delta b -> Delta i) -> IO ()) -> IO ())
    -> IO i -> DeltaMap i a -> SignalInfo i a
make link gather map = unsafePerformIO $ do
    input <- gather
    ref <- newIORef SignalState {
        value = undeltaMap map input,
        props = [], outputs = [] }
    let info = SignalInfo {
        state = ref, process = \(di :: Delta i) -> do
            i <- gather
            cur <- value <$> readIORef ref
            return $ map i di (Just cur) }
    link (\signal proj ->
        modifyIORef (state signal) (\stateVal ->
            stateVal { props = SignalProp proj info : props stateVal }))
    return info

-- | Maps a signal.
map :: (HasDelta a, HasDelta b) => DeltaMap a b -> Signal r a -> Signal r b
map map (Const x) = Const $ undeltaMap map x
map map (Signal info) = Signal $ make
    (\mkDep -> mkDep info id)
    (value <$> readIORef (state info))
    map

-- | A procedure which can manipulate signals in the context @r@.
newtype ReactT r m a = ReactT { runReactT' :: m a }
    deriving (Functor, Applicative, Monad)
instance MonadTrans (ReactT r) where
    lift = ReactT

-- | Runs a 'ReactT'.
runReactT :: forall m a. (forall r. ReactT r m a) -> m a
runReactT x = runReactT' (x :: ReactT r m a)

-- | A procedure which can manipulate signals.
type React r = ReactT r Identity

-- | Runs a 'React'.
runReact :: (forall r. React r a) -> a
runReact x = runIdentity $ runReactT x

-- | The input type for the given context.
newtype Input r a = Input (IORef (SignalState a))

-- | The output type for the given context.
newtype Output r a = Output (IORef (a, Delta a))

-- | Creates a new input signal, returning the input and the signal.
input :: (Monad m) => a -> ReactT r m (Input r a, Signal r a)
input cur = ReactT $ return res where
    ref = unsafePerformIO $ newIORef SignalState {
        value = cur, props = [], outputs = [] }
    res = (Input ref, Signal SignalInfo {
        state = ref,
        process = undefined })

-- | Updates the value of the signal associated with the given input, returning
-- the new value.
update :: (Monad m, HasDelta a) => Input r a -> Delta a -> ReactT r m a
update (Input state) delta = ReactT $ unsafePerformIO $ do
    change state delta
    cur <- value <$> readIORef state
    return $ return cur

-- | Creates a new output, returning both the output and the current value.
output :: (Monad m, HasDelta a) => Signal r a -> ReactT r m (Output r a, a)
output (Const value) = ReactT $ return
    (Output $ unsafePerformIO $ newIORef (value, keep), value)
output (Signal info) = ReactT $ return $ unsafePerformIO $ do
    let ref = state info
    stateVal <- readIORef ref
    res <- newIORef (value stateVal, keep)
    writeIORef ref $ stateVal { outputs = res : outputs stateVal }
    return (Output res, value stateVal)

-- | Returns the 'Delta' of an output since the last time look was used (or
-- created), along with the current value.
look :: (Monad m, HasDelta a) => Output r a -> ReactT r m (Delta a, a)
look (Output res) = ReactT $ unsafePerformIO $ do
    (last, delta) <- readIORef res
    let cur = apply delta last
    writeIORef res (cur, keep)
    return $ return (delta, cur)
