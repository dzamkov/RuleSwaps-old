{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Terminal.Figure.Flow (
    Flow,
    FlowLayout (..),
    FlowArea (..),
    PFlow,
    PFlowLayout (..),
    tightText,
    space,
    (+++),
    text
) where

import Terminal.Draw hiding (space)
import qualified Terminal.Draw as Draw
import Terminal.Figure.Core

-- | The layout type for a flowed figure, one which is linear and horizontal
-- with the ability to be broken up into parts, much like text.
data Flow

type instance Layout Flow = FlowLayout
type instance Placement Flow = To FlowArea PFlow

-- | The layout information provided by a flow.
data FlowLayout = FlowLayout {

    -- | The minimum prefered width of the flow, in the case where it is
    -- broken up wherever it is "okay".
    minWidth :: Width,

    -- | The maximum possible width of the flow, as in the case where it is
    -- not broken up at all.
    maxWidth :: Width }

-- | Describes an area that a flow can be placed in.
data FlowArea = FlowArea {

    -- | The width of the flow area.
    width :: Width,

    -- | The initial indentation of the flow.
    indent :: X }

-- | The layout type for a flowed figure which has a known 'FlowArea'. The
-- break points and final width of the flow have been decided, but the absolute
-- position and background color have not.
data PFlow

type instance Layout PFlow = PFlowLayout
type instance Placement PFlow = To (FullColor, Point) Static

-- | The layout information provided by a flow with known 'FlowArea'.
data PFlowLayout = PFlowLayout {

    -- | The point (in 'area' coordinates) where the remainder of the flow
    -- (if any) should start.
    end :: Point }

instance HasEmpty PFlow where
    empty = Figure {
        layout = PFlowLayout (0, 0),
        place = \(To _) -> empty }
instance HasEmpty Flow where
    empty = Figure {
        layout = FlowLayout {
            minWidth = 0,
            maxWidth = 0 },
        place = \(To _) -> empty }
instance CanTest Flow where
    test fig = do
        let area = FlowArea {
            width = testWidth,
            indent = 0 }
        let figP = place fig $ To area
        let back = (Dull, Magenta)
        let context = (back, (0, 0))
        let (_, endY) = end $ layout figP
        runDrawInline (endY + 1) $ draw $ place figP $ To context

-- | A flowed figure for text that will not be broken unless necessary.
tightText :: FullColor -> String -> Figure Flow
tightText _ [] = empty
tightText fore text = res where
    size = length text
    res = Figure {
        layout = FlowLayout {
            minWidth = size,
            maxWidth = size },
        place = \(To area) -> withArea area }
    withArea (FlowArea { .. }) = res where
        rem = width - indent
        (end, inline) = case (size <= rem, size <= width) of
            (True, _) -> ((indent + size, 0), True)
            (False, True) -> ((size, 1), False)
            (False, False) ->
                ((mod (indent + size) width,
                div (indent + size) width), True)
        res :: Figure PFlow
        res = Figure {
            layout = PFlowLayout end,
            place = \(To context) -> withContext context }
        withContext (back, (x, y)) = static draw where
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
    res = Figure {
        layout = FlowLayout {
            minWidth = 0,
            maxWidth = size },
        place = \(To area) -> withArea area }
    withArea (FlowArea { .. }) = res where
        (end, len) = case () of
            _ | indent == 0 -> ((0, 0), 0)
            _ | indent + size >= width -> ((0, 1), width - indent)
            _ -> ((indent + size, 0), size)
        res :: Figure PFlow
        res = Figure {
            layout = PFlowLayout end,
            place = \(To context) -> withContext context }
        withContext (back, (x, y)) = static $ case len of
            0 -> Draw.none
            len -> Draw.space back (x + indent, y) len

-- | Concatenates flowed figures.
(+++) :: Figure Flow -> Figure Flow -> Figure Flow
(+++) x y | maxWidth (layout x) == 0 = y
(+++) x y | maxWidth (layout y) == 0 = x
(+++) x y = res where
    (xL, yL) = (layout x, layout y)
    res = Figure {
        layout = FlowLayout {
            maxWidth = maxWidth xL + maxWidth yL,
            minWidth = max (minWidth xL) (minWidth yL) },
        place = \(To area) -> withArea area }
    withArea xArea@(FlowArea { .. }) = res where
        xP = place x $ To xArea
        (xEndX, xEndY) = end $ layout xP
        yArea = FlowArea { width = width, indent = xEndX }
        yP = place y $ To yArea
        (yEndX, yEndY) = end $ layout yP
        xyEnd = (yEndX, xEndY + yEndY)
        res :: Figure PFlow
        res = Figure {
            layout = PFlowLayout xyEnd,
            place = \(To context) -> withContext context }
        withContext (back, (x, y)) = static
            (draw (place xP $ To (back, (x, y))) |%
            draw (place yP $ To (back, (x, y + xEndY))))

-- | A flowed figure for text, with breaking spaces.
text :: FullColor -> String -> Figure Flow
text fore = breakSpace 0 where
    breakWord a [] = tightText fore (reverse a)
    breakWord a (' ' : xs) = tightText fore (reverse a) +++ breakSpace 1 xs
    breakWord a (x : xs) = breakWord (x : a) xs
    breakSpace n [] = space n
    breakSpace n (' ' : xs) = breakSpace (n + 1) xs
    breakSpace n (x : xs) = space n +++ breakWord [x] xs
