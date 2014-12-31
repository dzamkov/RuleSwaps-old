{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Terminal.Figure.Flow (
    Flow,
    FlowLayout (..),
    FlowArea (..),
    tightText,
    space,
    (+++),
    text
) where

import Stride
import Terminal.Draw hiding (space)
import qualified Terminal.Draw as Draw
import Terminal.Figure.Core
import Control.Applicative hiding (empty)

-- | The layout type for a flowed figure, one which is linear and horizontal
-- with the ability to be broken up into parts, much like text.
data Flow

type instance Layout Flow = FlowLayout
type instance Placement Flow = (FlowArea, FullColor, Point)
type instance Bonus Flow = Point

-- | The layout information provided by a flow.
data FlowLayout = FlowLayout {

    -- | The minimum prefered width of the flow, in the case where it is
    -- broken up wherever it is "okay".
    minWidth :: Width,

    -- | The maximum possible width of the flow, as in the case where it is
    -- not broken up at all.
    maxWidth :: Width,

    -- | Given the potential area for this flow, determines where the
    -- remainder of the flow should start.
    trialPlace :: FlowArea -> Point }

-- | Describes an area that a flow can be placed in.
data FlowArea = FlowArea {

    -- | The width of the flow area.
    width :: Width,

    -- | The initial indentation of the flow.
    indent :: X }

-- | Constructs a flow figure given its 'minWidth', 'maxWidth', and a function
-- which sets the area for the flow.
flow :: Width -> Width -> (FlowArea -> (Point, FullColor -> Point -> Draw))
    -> Figure Flow
flow minWidth maxWidth withArea = Figure {
    layout = FlowLayout {
        minWidth = minWidth,
        maxWidth = maxWidth,
        trialPlace = fst . withArea },
    place = \(area, back, offset) ->
        let (end, withContext) = withArea area
        in (withContext back offset, end) }

instance HasEmpty Flow where
    empty = flow 0 0 (\area -> ((indent area, 0), \_ _ -> none))
instance CanTest Flow where
    test fig = do
        let area = FlowArea {
            width = testWidth,
            indent = 0 }
        let back = (Dull, Magenta)
        let (draw, (_, endY)) = place fig (area, back, (0, 0))
        runDrawInline (endY + 1) draw

-- | A flowed figure for text that will not be broken unless necessary.
tightText :: FullColor -> String -> Figure Flow
tightText _ [] = empty
tightText fore text = res where
    size = length text
    res = flow size size withArea
    withArea (FlowArea { .. }) = res where
        rem = width - indent
        (end, inline) = case (size <= rem, size <= width) of
            (True, _) -> ((indent + size, 0), True)
            (False, True) -> ((size, 1), False)
            (False, False) ->
                ((mod (indent + size) width,
                div (indent + size) width), True)
        res = (end, withContext)
        withContext back (x, y) = draw where
            appr = (back, fore)
            cont accum y text =
                case text of
                    [] -> accum
                    text ->
                        let (bText, tail) = splitAt width text
                        in cont (accum |% Draw.string appr (x, y) bText)
                            (y + 1) tail
            (bText, tail) = splitAt rem text
            draw = if inline
                then cont (Draw.string appr (x + indent, y) bText) (y + 1) tail
                else Draw.space back (x + indent, y) rem |%
                    Draw.string appr (x, y + 1) text

-- | A flowed figure for a breaking space of the given size.
space :: Int -> Figure Flow
space 0 = empty
space size = res where
    res = flow 0 size withArea
    withArea (FlowArea { .. }) = res where
        (end, len) = case () of
            _ | indent == 0 -> ((0, 0), 0)
            _ | indent + size >= width -> ((0, 1), width - indent)
            _ -> ((indent + size, 0), size)
        res = (end, withContext)
        withContext back (x, y) = case len of
            0 -> Draw.none
            len -> Draw.space back (x + indent, y) len

(+++) :: (FigureLike f) => f Flow -> f Flow -> f Flow
(+++) a b = compose (\eval -> concat' <$> eval a <*> eval b) where
    concat' a b = res where
        (aL, bL) = (layoutS a, layoutS b)
        abL = (\aL bL -> FlowLayout {
            maxWidth = maxWidth aL + maxWidth bL,
            minWidth = max (minWidth aL) (minWidth bL),
            trialPlace = \aArea ->
                let (aEndX, aEndY) = trialPlace aL aArea
                    bArea = aArea { indent = aEndX }
                    (bEndX, bEndY) = trialPlace bL bArea
                in (bEndX, aEndY + bEndY) })
            <$> aL <*> bL
        res = figureS abL $ funS withAll
        withAll (aArea, back, (x, y)) = res where
            (aDraw, aEnd) = break2S $ placeS a <*> pure (aArea, back, (x, y))
            bContext = (\(aEndX, aEndY) ->
                    (aArea { indent = aEndX }, back, (x, aEndY + y)))
                <$> aEnd
            (bDraw, bEnd) = break2S $ placeS b <*> bContext
            abEnd = checkS $ (\(_, aEndY) (bEndX, bEndY) ->
                (bEndX, aEndY + bEndY)) <$> aEnd <*> bEnd
            res = plex2S (plusS aDraw bDraw) abEnd

-- | A flowed figure for text, with breaking spaces.
text :: FullColor -> String -> Figure Flow
text fore = breakSpace 0 where
    breakWord a [] = tightText fore (reverse a)
    breakWord a (' ' : xs) = tightText fore (reverse a) +++ breakSpace 1 xs
    breakWord a (x : xs) = breakWord (x : a) xs
    breakSpace n [] = space n
    breakSpace n (' ' : xs) = breakSpace (n + 1) xs
    breakSpace n (x : xs) = space n +++ breakWord [x] xs
