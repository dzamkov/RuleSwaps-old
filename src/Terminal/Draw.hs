{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Terminal.Draw (
    cursorUp,
    cursorDown,
    cursorForward,
    cursorBackward,
    changePosition,
    changeBackground,
    Appearance,
    changeAppearance,
    setAppearance,
    defaultAppearance,
    changeState,
    Draw,
    runDraw,
    none,
    string,
    space,
    hline,
    vline,
    fill,
    clip,
    withAppearance,
    runDrawInline
) where

import Terminal.Context
import System.Console.ANSI hiding (Color)
import System.IO (hFlush, stdout)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid
import Control.Monad

-- | Sets the position of the cursor given the current position.
changePosition :: Width -> Point -> Point -> IO ()
changePosition (Width width) old new = do
    let (Width oldX', Height oldY') = old
    let (Width newX, Height newY) = new
    let oldX = oldX' `rem` width
    let oldY = oldY' + (oldX' `div` width)
    when (newY < oldY) $ cursorUp (oldY - newY)
    when (newY > oldY) $ cursorDown (newY - oldY)
    when (newX /= oldX) $ setCursorColumn newX

-- | Sets the color of the background for future text written, given the
-- current background.
changeBackground :: Color -> Color -> IO ()
changeBackground old new@(Color newI newC) =
    when (old /= new) $ setSGR [SetColor Background newI newC]

-- | Sets the appearance of future written text given the current appearance
-- of text.
changeAppearance :: Appearance -> Appearance -> IO ()
changeAppearance (oldB, oldF) new =
    let (newB@(Color newBI newBC), newF@(Color newFI newFC)) = new
        ifc c l ni nc = if c then Just $ SetColor l ni nc else Nothing
        coms = catMaybes [
             ifc (oldB /= newB) Background newBI newBC,
             ifc (oldF /= newF) Foreground newFI newFC]
    in unless (null coms) $ setSGR coms

-- | Sets the appearance of future written text.
setAppearance :: Appearance -> IO ()
setAppearance (Color bi bc, Color fi fc) =
    setSGR [SetColor Background bi bc,
        SetColor Foreground fi fc]

-- | Describes the state of the terminal cursor, along with the width of the
-- terminal.
type State = (Width, Point, Appearance)

-- | Sets the state of the terminal cursor given the current state.
changeState :: State -> State -> IO ()
changeState (_, oldPos, oldAppr) (width, newPos, newAppr) = do
    changePosition width oldPos newPos
    changeAppearance oldAppr newAppr

-- | A primitive operation which draws something to the terminal.
data DrawOp
    = String Appearance Point String
    | Space Color Point Width
    deriving (Eq, Ord, Show)

-- | Performs a drawing operation on the current terminal.
runDrawOp :: DrawOp -> State -> IO State
runDrawOp (String appr (x, y) str) st@(width, _, _) = do
    changeState st (width, (x, y), appr)
    putStr str
    return (width, (x + Width (length str), y), appr)
runDrawOp (Space back (x, y) wid) (width, oldPos, (oldBack, oldFore)) = do
    changePosition width oldPos (x, y)
    changeBackground oldBack back
    putStr $ replicate (cells wid) ' '
    return (width, (x + wid, y), (back, oldFore))

-- | A procedure which draws something to the terminal.
newtype Draw = Draw [DrawOp] deriving (Eq, Ord, Show, Monoid)

-- | Performs a drawing procedure on the current terminal.
runDraw :: Draw -> State -> IO State
runDraw (Draw ops) st = do
    nSt <- foldM (flip runDrawOp) st ops
    hFlush stdout
    return nSt

-- | Does no drawing.
none :: Draw
none = Draw []

-- | Draws a string with the given appearance to the given point.
string :: Appearance -> Point -> String -> Draw
string _ _ [] = none
string appr point str = Draw [String appr point str]

-- | Draws a horizontal space with the given back color and width to the given
-- point.
space :: Color -> Point -> Width -> Draw
space _ _ (Width 0) = none
space back point wid = Draw [Space back point wid]

-- | Draws a horizontal line consisting of the given characters.
hline :: Appearance -> Char -> Point -> Width -> Draw
hline _ _ _ (Width 0) = none
hline appr ch point (Width wid) = Draw [String appr point $ replicate wid ch]

-- | Draws a vertical line consisting of the given characters.
vline :: Appearance -> Char -> Point -> Height -> Draw
vline _ _ _ (Height 0) = none
vline appr ch (x, y) (Height height) = Draw $
    map (\offset -> String appr (x, y + Height offset) [ch])
    [0 .. height - 1]

-- | Fills an area with colored space.
fill :: Color -> Point -> Width -> Height -> Draw
fill _ _ 0 _ = none
fill _ _ _ 0 = none
fill back (x, y) width height = space back (x, y) width <>
    fill back (x, y + 1) width (height - 1)

-- | Restricts a draw operation to the given rectangular region.
clip :: Point -> Width -> Height -> Draw -> Draw
clip (left, top) width height (Draw ops) = res where
    bottom = top + height
    right = left + width
    clipOp (String _ (_, y) _) | y < top || y >= bottom = Nothing
    clipOp (String appr (x, y) str) =
        case drop (cells (left - x)) $ take (cells (right - x)) str of
            [] -> Nothing
            nStr -> Just $ String appr (max left x, y) nStr
    clipOp (Space _ (_, y) _) | y < top || y >= bottom = Nothing
    clipOp (Space _ (x, _) size) | x + size <= left || x >= right = Nothing
    clipOp (Space back (x, y) size) = Just $ Space back (max left x, y)
        (min (x + size) right - max left x)
    res = Draw $ mapMaybe clipOp ops

-- | Overrides the appearance of a drawing operation. Useful for debugging
withAppearance :: Appearance -> Draw -> Draw
withAppearance appr (Draw ops) = Draw $ map f ops where
    f (String _ offset str) = String appr offset str
    f (Space _ offset wid) = Space (fst appr) offset wid

-- | Performs a drawing operation on the current terminal within a REPL
-- output section. This assumes that the origin of the drawing is at the top-
-- left corner of the section.
runDrawInline :: Height -> Draw -> IO ()
runDrawInline height draw = do
    setAppearance defaultAppearance
    replicateM_ (cells height) $ putStrLn ""
    cursorUp $ cells height
    st <- runDraw draw (maxBound, (Width 0, Height 0), defaultAppearance)
    changeState st (maxBound, (Width 0, height), defaultAppearance)
