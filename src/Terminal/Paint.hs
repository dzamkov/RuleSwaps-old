{-# LANGUAGE TypeFamilies #-}
module Terminal.Paint (
    Paint,
    toPaint,
    fromPaint,
    mix,
    over,
    signals,
    runPaint
) where

import Reactive
import qualified Reactive.IO as IO
import Terminal.Context
import Terminal.Draw
import qualified System.Console.Terminal.Size as Size
import Data.IORef
import Data.Monoid
import Control.Monad (when, replicateM_)
import Control.Applicative

-- | Given that @f@ is a behavior constructor, this represents a
-- time-varying picture, similar to @f Draw@. Unlike @f Draw@, 'Paint'
-- allows efficient partial updates.
data Paint f
    = ToPaint (f Draw)
    | Mix (Paint f) (Paint f)
    | Over (Paint f) (Paint f)

-- | Converts a draw signal to a paint.
toPaint :: f Draw -> Paint f
toPaint = ToPaint

-- | Combines two paints assuming that they don't overlap at any point in time.
mix :: Paint f -> Paint f -> Paint f
mix = Mix

-- | Combines two paints. Where overlap occurs, the first will take precedence.
over :: Paint f -> Paint f -> Paint f
over = Over

-- | Converts a paint into a draw behavior, discarding the information needed
-- to perform partial updates.
fromPaint :: (Applicative f) => Paint f -> f Draw
fromPaint (ToPaint signal) = signal
fromPaint (Mix x y) = (<>) <$> fromPaint x <*> fromPaint y
fromPaint (Over x y) = (<>) <$> fromPaint y <*> fromPaint x

-- | Gets a 'Draw' behavior for a paint, along with an event which provides
-- partial updates. Accumulating the partial update event should yield the
-- draw behavior.
signals :: (ReactiveDiscrete e f) => Paint f -> (f Draw, e Draw)
signals (ToPaint source) = (source, changes source)
signals (Mix x y) = (rB, rC) where
    (xB, xC) = signals x
    (yB, yC) = signals y
    rB = (<>) <$> xB <*> yB
    rC = xC `union` yC
signals (Over hi lo) = (rB, rC) where
    (hiB, hiC) = signals hi
    (loB, loC) = signals lo
    rB = (<>) <$> loB <*> hiB
    rC = hiC `union` (flip (<>) <$> hiB <@> loC)

-- | Displays a paint on the current terminal, keeping it updated as needed.
-- The size of the terminal may be used to generate the paint.
runPaint :: (IO.Behavior (Width, Height) -> Paint IO.Behavior) -> IO ()
runPaint source = do
    let getSize = do
        Just win <- Size.size
        return (Width $ Size.width win, Height $ Size.height win)
    curSize <- getSize
    (sizeChanged, changeSize) <- IO.newEvent
    let size = stepper curSize sizeChanged
    let paint = source size
    let (paintB, paintC) = signals paint

    let Height height = snd curSize
    replicateM_ height $ putStrLn ""
    cursorUp height
    setAppearance defaultAppearance
    let st = (fst curSize, (0, 0), defaultAppearance)
    curDraw <- IO.value paintB
    nSt <- runDraw curDraw st
    stRef <- newIORef nSt

    let checkSize = do
        curSize <- IO.value size
        nSize <- getSize
        when (curSize /= nSize) $ changeSize nSize
    let update draw = do
        st <- readIORef stRef
        nSt <- runDraw draw st
        writeIORef stRef nSt
        checkSize
    IO.registerEternal paintC update -- TODO: possible race condition
