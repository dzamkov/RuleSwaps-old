{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Terminal.Draw (
    X, Y,
    Point,
    changePosition,
    Width,
    Height,
    Color (..),
    ColorIntensity (..),
    FullColor,
    changeBackground,
    Appearance,
    changeAppearance,
    setAppearance,
    defaultAppearance,
    Draw,
    runDraw,
    none,
    (|%), (|%|),
    dplus,
    string,
    space,
    hline,
    vline,
    clip,
    runDrawInline
) where

import Stride
import System.Console.ANSI
import Data.Maybe (catMaybes, mapMaybe)
import Control.Monad
import Control.Applicative

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
type FullColor = (ColorIntensity, Color)

-- | Sets the color of the background for future text written, given the
-- current background.
changeBackground :: FullColor -> FullColor -> IO ()
changeBackground old new@(newI, newC) =
    when (old /= new) $ setSGR [SetColor Background newI newC]

-- | Describes the appearance of a glyph on the terminal.
type Appearance = (FullColor, FullColor)

-- | Sets the appearance of future written text given the current appearance
-- of text.
changeAppearance :: Appearance -> Appearance -> IO ()
changeAppearance (oldB, oldF) (newB@(newBI, newBC), newF@(newFI, newFC)) =
    let ifc c l ni nc = if c then Just $ SetColor l ni nc else Nothing
    in let coms = catMaybes [
             ifc (oldB /= newB) Background newBI newBC,
             ifc (oldF /= newF) Foreground newFI newFC]
    in unless (null coms) $ setSGR coms

-- | Sets the appearance of future written text.
setAppearance :: Appearance -> IO ()
setAppearance ((bi, bc), (fi, fc)) =
    setSGR [SetColor Background bi bc,
        SetColor Foreground fi fc]

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

-- | A primitive operation which draws something to the terminal.
data DrawOp
    = String Appearance Point String
    | Space FullColor Point Width

-- | Performs a drawing operation on the current terminal.
runDrawOp :: DrawOp -> State -> IO State
runDrawOp (String appr (x, y) str) st = do
    changeState st ((x, y), appr)
    putStr str
    return ((x + length str, y), appr)
runDrawOp (Space back (x, y) wid) (oldPos, (oldB, oldF)) = do
    changePosition oldPos (x, y)
    changeBackground oldB back
    putStr $ replicate wid ' '
    return ((x + wid, y), (back, oldF))

-- | A procedure which draws something to the terminal.
newtype Draw = Draw [DrawOp]
data SDraw = SDraw Draw Draw
type instance Complex Draw = SDraw
instance IsStride SDraw where
    glue (SDraw i x) (SDraw _ y) = SDraw i (x |% y)
instance StrideRel SDraw Draw where
    start (SDraw i _) = i
    end (SDraw i a) = i |% a

-- | Performs a drawing procedure on the current terminal.
runDraw :: Draw -> State -> IO State
runDraw (Draw ops) st = foldM (flip runDrawOp) st ops

-- | Does no drawing.
none :: Draw
none = Draw []

-- | Combines two drawing operations. When overwrite is possible, drawings in
-- the second operation take precedence.
(|%) :: Draw -> Draw -> Draw
(|%) (Draw a) (Draw b) = Draw (a ++ b)

-- | Combines two drawing operations assuming overwrite is not possible.
(|%|) :: Draw -> Draw -> Draw
(|%|) = (|%)

-- | Combines two drawing operations within the context of a deltor, assuming
-- overwrite is not possible.
dplus :: (Deltor f) => f Draw -> f Draw -> f Draw
dplus x y = deltor (\eval -> combine <$> eval x <*> eval y) where
    combine (Stride xs xe) (Stay y) = Complex $ SDraw (xs |%| y) xe
    combine (Stay x) (Stride ys ye) = Complex $ SDraw (ys |%| x) ye
    combine (Complex (SDraw xs xd)) (Complex (SDraw ys yd)) =
        Complex $ SDraw (xs |%| ys) (xd |%| yd)
    combine x y = (|%|) <$> x <*> y

-- | Draws a string with the given appearance to the given point.
string :: Appearance -> Point -> String -> Draw
string _ _ [] = none
string appr point str = Draw [String appr point str]

-- | Draws a horizontal space with the given back color and width to the given
-- point.
space :: FullColor -> Point -> Width -> Draw
space _ _ 0 = none
space back point wid = Draw [Space back point wid]

-- | Draws a horizontal line consisting of the given characters.
hline :: Appearance -> Char -> Point -> Width -> Draw
hline _ _ _ 0 = none
hline appr ch point wid = Draw [String appr point $ replicate wid ch]

-- | Draws a vertical line consisting of the given characters.
vline :: Appearance -> Char -> Point -> Height -> Draw
vline _ _ _ 0 = none
vline appr ch (x, y) height = Draw $
    map (\offset -> String appr (x, y + offset) [ch])
    [0 .. height - 1]

-- | Restricts a draw operation to the given rectangular region.
clip :: Point -> Width -> Height -> Draw -> Draw
clip (left, top) width height (Draw ops) = res where
    bottom = top + height
    right = left + width
    clipOp (String _ (_, y) _) | y < top || y >= bottom = Nothing
    clipOp (String appr (x, y) str) =
        case drop (left - x) $ take (right - x) str of
            [] -> Nothing
            nStr -> Just $ String appr (max left x, y) nStr
    clipOp (Space _ (_, y) _) | y < top || y >= bottom = Nothing
    clipOp (Space _ (x, _) size) | x + size <= left || x >= right = Nothing
    clipOp (Space back (x, y) size) = Just $ Space back (max left x, y)
        (min (x + size) right - max left x)
    res = Draw $ mapMaybe clipOp ops

-- | Performs a drawing operation on the current terminal within a REPL
-- output section. This assumes that the origin of the drawing is at the top-
-- left corner of the section.
runDrawInline :: Height -> Draw -> IO ()
runDrawInline height draw = do
    setAppearance defaultAppearance
    replicateM_ height $ putStrLn ""
    cursorUp height
    st <- runDraw draw ((0, 0), defaultAppearance)
    changeState st ((0, height), defaultAppearance)
