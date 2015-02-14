{-# LANGUAGE TypeFamilies #-}
module Terminal.Paint (
    Paint,
    toPaint,
    fromPaint,
    mix,
    over,
    updates,
    runPaint
) where

import Terminal.Context
import Terminal.Draw
import qualified System.Console.Terminal.Size as Size
import System.Console.ANSI (cursorUp)
import Data.IORef
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Monoid
import Control.Monad (replicateM_)

-- | Given that @f@ is a signal constructor, this represents a
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

-- | Converts a paint into a draw signal, discarding the information needed
-- to perform partial updates.
fromPaint :: (Applicative f) => Paint f -> f Draw
fromPaint (ToPaint signal) = signal
fromPaint (Mix x y) = (<>) <$> fromPaint x <*> fromPaint y
fromPaint (Over x y) = (<>) <$> fromPaint y <*> fromPaint x

-- | Gets an event source for a paint providing partial updates for the
-- underlying draw 'Behavior'. Use 'fromPaint' to get the initial 'Draw' to
-- which updates can be applied to.
updates :: Frameworks t => Paint (Behavior t)
    -> Moment t (Event t (Future Draw))
updates (ToPaint signal) = changes signal
updates (Mix x y) = union <$> updates x <*> updates y
updates (Over hi lo) = do
    hiUpdates <- updates hi
    loUpdates <- updates lo
    let nLoUpdates = (\hi fLo -> (<> hi) <$> fLo) <$>
          fromPaint hi <@> loUpdates
    return $ union hiUpdates nLoUpdates

-- | Performs the necessary 'Moment' operations such that, while actuated, the
-- network will keep the current terminal updated with the given paint.
-- Returns a behavior that provides the size of the terminal.
runPaint :: Frameworks t => Paint (Behavior t)
    -> Moment t (Behavior t (Width, Height))
runPaint paint = do
    (sizeChanged, changeSize) <- newEvent -- TODO: polling for changeSize
    let getSize = do
        Just win <- Size.size
        return (Width $ Size.width win, Height $ Size.height win)
    curSize <- liftIO getSize
    let size = stepper curSize sizeChanged
    curDraw <- initial $ fromPaint paint
    stRef <- liftIO $ newIORef undefined
    liftIOLater $ do
        let Height height = snd curSize
        replicateM_ height $ putStrLn ""
        cursorUp height
        setAppearance defaultAppearance
        let st = (fst curSize, (0, 0), defaultAppearance)
        nSt <- runDraw curDraw st
        writeIORef stRef nSt
    let update draw = do
        st <- readIORef stRef
        nSt <- runDraw draw st
        writeIORef stRef nSt
    paintUpdates <- updates paint
    reactimate' $ (update <$>) <$> paintUpdates
    return size
