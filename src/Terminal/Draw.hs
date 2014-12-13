module Terminal.Draw (
    X, Y,
    Point,
    Width,
    Height,
    CompleteColor,
    Appearance,
    defaultAppearance,
    Draw,
    runDraw,
    (|%),
    drawStr,
    drawSpace
) where

import System.Console.ANSI
import Data.Maybe (catMaybes)
import Control.Monad (when, unless, (>=>))

-- | Describes the X offset of a point from the left edge of the terminal.
type X = Int

-- | Describes the Y offset of a point from the top of the terminal.
type Y = Int

-- | Describes a point on the terminal.
type Point = (X, Y)

-- | Sets the position of the cursor given the current position.
changePosition :: Point -> Point -> IO ()
changePosition (oldX, oldY) (newX, newY) = do
    when (newY < oldY) $ cursorUp (oldY - newY)
    when (newY > oldY) $ cursorDown (newY - oldY)
    when (newX < oldX) $ cursorBackward (oldX - newX)
    when (newX > oldX) $ cursorForward (newX - oldX)

-- | Describes the width of a terminal area.
type Width = Int

-- | Describes the height of a terminal area.
type Height = Int

-- | Describes a color that the terminal can display.
type CompleteColor = (ColorIntensity, Color)

-- | Sets the color of the background for future text written, given the
-- current background.
changeBackground :: CompleteColor -> CompleteColor -> IO ()
changeBackground old new@(newI, newC) =
    when (old /= new) $ setSGR [SetColor Background newI newC]

-- | Describes the appearance of a glyph on the terminal.
type Appearance = (CompleteColor, CompleteColor)

-- | Sets the appearance of future written text given the current appearance
-- of text.
changeAppearance :: Appearance -> Appearance -> IO ()
changeAppearance (oldB, oldF) (newB@(newBI, newBC), newF@(newFI, newFC)) =
    let ifc c l ni nc = if c then Just $ SetColor l ni nc else Nothing
    in let coms = catMaybes [
             ifc (oldB /= newB) Background newBI newBC,
             ifc (oldF /= newF) Foreground newFI newFC]
    in unless (null coms) $ setSGR coms

-- | The default appearance for some terminal.
defaultAppearance :: Appearance
defaultAppearance = ((Dull, Black), (Dull, White))

-- | Describes the state of the terminal cursor.
type State = (Point, Appearance)

-- | Sets the state of the terminal cursor given the current state.
changeState :: State -> State -> IO ()
changeState (oldPos, oldAppr) (newPos, newAppr) = do
    changePosition oldPos newPos
    changeAppearance oldAppr newAppr

-- | A procedure which draws something to the terminal.
newtype Draw = Draw {

    -- | Performs a drawing operation on the current terminal.
    runDraw :: State -> IO State }

-- | Combines two drawing operations. When overwrite is possible, drawings in
-- the second operation take precedence.
(|%) :: Draw -> Draw -> Draw
(|%) (Draw a) (Draw b) = Draw (a >=> b)

-- | Draws a string with the given appearance to the given point.
drawStr :: Appearance -> Point -> String -> Draw
drawStr appr (x, y) str = Draw $ \st -> do
    changeState st ((x, y), appr)
    putStr str
    return ((x + length str, y), appr)

-- | Draws a horizontal space with the given back color and width to the given
-- point.
drawSpace :: CompleteColor -> Point -> Width -> Draw
drawSpace back (x, y) wid = Draw $ \(oldPos, (oldB, oldF)) -> do
    changePosition oldPos (x, y)
    changeBackground oldB back
    putStr $ replicate wid ' '
    return ((x + wid, y), (back, oldF))
