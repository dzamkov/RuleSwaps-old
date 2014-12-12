{-# LANGUAGE GADTs #-}
module Terminal.Figure (
    X, Y,
    Width,
    Height,
    Point,
    changePosition,
    CompleteColor,
    Appearance,
    changeAppearance,
    defaultAppearance,
    State,
    changeState,
    Flow (..),
    Block (..),
    Dock (..),
    FigureDrawer (..),
    Figure (..),
    tightText,
    space,
    (+++),
    text,
    test
) where

import System.Console.ANSI
import Data.Maybe (catMaybes)
import Control.Monad (when, unless)

-- | Describes the X offset of a point from the left edge of the terminal.
type X = Int

-- | Describes the Y offset of a point from the top of the terminal.
type Y = Int

-- | Describes a point on the terminal.
type Point = (X, Y)

-- | Describes the width of a terminal area.
type Width = Int

-- | Describes the height of a terminal area.
type Height = Int

-- | Sets the position of the cursor given the current position.
changePosition :: Point -> Point -> IO ()
changePosition (oldX, oldY) (newX, newY) = do
    when (newY < oldY) $ cursorUp (oldY - newY)
    when (newY > oldY) $ cursorDown (newY - oldY)
    when (newX < oldX) $ cursorBackward (oldX - newX)
    when (newX > oldX) $ cursorForward (newX - oldX)

-- | Describes a color that the terminal can display.
type CompleteColor = (ColorIntensity, Color)

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

-- | Contains sizing information for a flowed figure, that is, a figure which
-- is linear and horizontal, and can be broken up into parts, much like text.
data Flow = Flow {

    -- | The maximum possible width of the flow, in the case where it is not
    -- broken up.
    maxWidth :: Width,

    -- | The minimum prefered width of the flow, in the case where it is
    -- broken up wherever it is "okay".
    minWidth :: Width }

-- | Contains sizing information for a block figure, that is, a figure which
-- takes up a rectangular area of a fixed size.
data Block = Block {

    -- | The width of the block.
    width :: Width,

    -- | The height of the block.
    height :: Height }

-- | Contains the non-existant sizing information for a docked figure, that is,
-- a figure which takes up a rectangular area of a given size.
data Dock = Dock

-- | A procedure which draws a figure.
data FigureDrawer a where

    -- | Draws a flow figure given the horizontal bounds of the flow area.
    FlowDrawer :: (X -> Width -> State -> IO State) -> FigureDrawer Flow

-- | A figure that can be drawn to a terminal.
data Figure a = Figure { size :: a, drawer :: FigureDrawer a }

-- | Used to implement 'empty'
class HasEmpty a where

    -- | An empty figure that takes up minimal space.
    empty :: Figure a

instance HasEmpty Flow where
    empty = Figure {
        size = Flow { maxWidth = 0, minWidth = 0 },
        drawer = FlowDrawer $ \_ _ -> return }

-- | A flowed figure for text that will not be broken unless necessary.
tightText :: Appearance -> String -> Figure Flow
tightText _ [] = empty
tightText appr text =
    let width = length text
    in Figure {
        size = Flow { maxWidth = width, minWidth = width },
        drawer = FlowDrawer $ \start size ((x, y), oldAppr) -> do
            changeAppearance oldAppr appr
            let rem = start + size - x
            let cont y text =
                  case splitAt size text of
                    (text, []) -> do
                        putStr text
                        return ((start + length text, y), appr)
                    (head, tail) -> do
                        putStr head
                        changePosition (start + size, y) (start, y + 1)
                        cont (y + 1) tail
            case (width <= rem, width <= size) of
                (True, _) -> do
                    putStr text
                    return ((x + width, y), appr)
                (False, True) -> do
                    changePosition (x, y) (start, y + 1)
                    cont (y + 1) text
                (False, False) -> cont y text }

-- | A flowed figure for a breaking space of the given size.
space :: Int -> Figure Flow
space 0 = empty
space width = Figure {
    size = Flow { maxWidth = width, minWidth = 0 },
    drawer = FlowDrawer $ \start size ((x, y), appr) -> do
        let nPos = if x + width >= start + size
              then (start, y + 1)
              else (x + width, y)
        changePosition (x, y) nPos
        return (nPos, appr) }

-- | Concatenates flowed figures.
(+++) :: Figure Flow -> Figure Flow -> Figure Flow
(+++) x y | maxWidth (size x) == 0 = y
(+++) x y | maxWidth (size y) == 0 = x
(+++) x y = Figure {
    size = Flow {
        maxWidth = maxWidth (size x) + maxWidth (size y),
        minWidth = max (minWidth $ size y) (minWidth $ size y) },
    drawer = FlowDrawer $ \start size st -> do
        nSt <- (\(FlowDrawer xDraw) -> xDraw start size st) $ drawer x
        (\(FlowDrawer yDraw) -> yDraw start size nSt) $ drawer y }

-- | A flowed figure for text, with breaking spaces.
text :: Appearance -> String -> Figure Flow
text appr = breakSpace 0 where
    breakWord a [] = tightText appr (reverse a)
    breakWord a (' ' : xs) = tightText appr (reverse a) +++ breakSpace 1 xs
    breakWord a (x : xs) = breakWord (x : a) xs
    breakSpace n [] = space n
    breakSpace n (' ' : xs) = breakSpace (n + 1) xs
    breakSpace n (x : xs) = space n +++ breakWord [x] xs

-- | Used to implement 'test'
class HasTestDrawer a where
    draw :: FigureDrawer a -> State -> IO State
instance HasTestDrawer Flow where
    draw (FlowDrawer x) = x 0 60

-- | Test draws a figure.
test :: (HasTestDrawer a) => Figure a -> IO ()
test fig = do
    let st = ((0, 0), defaultAppearance)
    nSt <- draw (drawer fig) st
    changeState nSt ((0, 20), defaultAppearance)