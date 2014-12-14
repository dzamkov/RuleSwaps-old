{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Terminal.Figure (
    Flow,
    Block,
    Dock,
    Layout (..),
    Placement (..),
    Bonus,
    Figure (..),
    tightText,
    space,
    FlowLike ((+++)),
    text,
    test
) where

import System.Console.ANSI
import Terminal.Draw

-- | Marks a figure as flowed, that is, linear and horizontal with the ability
-- to be broken up into parts, much like text.
data Flow

-- | Marks a figure as a block, that is, taking up a fixed-size rectangular
-- area.
data Block

-- | Marks a figure as docked, that is, taking up a variable-size rectangular
-- area.
data Dock

-- | Describes the sizing information contained with a figure with the given
-- mark.
data family Layout a

-- | Describes the placement information needed to draw a figure with the given
-- mark.
data family Placement a

-- | Contains potentially useful information that was computed during the
-- drawing of a figure.
type family Bonus a

-- | Contains sizing information for a flowed figure.
data instance Layout Flow = FlowLayout {

    -- | The maximum possible width of the flow, in the case where it is not
    -- broken up.
    flowMaxWidth :: Width,

    -- | The minimum prefered width of the flow, in the case where it is
    -- broken up wherever it is "okay".
    flowMinWidth :: Width,

    -- | When given a possible placement of this flow, this will the return
    -- the point where the remainder of the flow (if any) should start. If
    -- the point is past the vertical limit, it no longer needs to be
    -- accurate.
    flowTrialPlace :: Placement Flow -> Point }

-- | Contains sizing information for a block figure.
data instance Layout Block = BlockLayout {

    -- | The width of the block.
    blockWidth :: Width,

    -- | The height of the block.
    blockHeight :: Height }

-- | Contains the non-existant sizing information for a docked figure.
data instance Layout Dock = DockLayout

-- | Contains the placement information for a flowed figure.
data instance Placement Flow = FlowPlacement {

    -- | The color used to fill empty space in the flow (spaces and breaks).
    flowBack :: CompleteColor,

    -- | The horizontal offset of the flow area.
    flowLeft :: X,

    -- | The width of the flow area.
    flowWidth :: Width,

    -- | The initial position of the cursor in the flow area.
    flowStart :: Point,

    -- | The vertical offset at which drawing should stop.
    flowLimit :: Y }

-- | The point at which the remainder of the flow starts.
type instance Bonus Flow = Point

-- | A figure that can be drawn to a terminal.
data Figure a = Figure {

    -- | Contains sizing and layout information for the figure.
    layout :: Layout a,

    -- | Creates a procedure that can be used to draw this figure with the
    -- given placement.
    prepare :: Placement a -> (Draw, Bonus a) }

-- | Used to implement 'empty'
class HasEmpty a where

    -- | An empty figure that takes up minimal space.
    empty :: Figure a

instance HasEmpty Flow where
    empty = Figure {
        layout = FlowLayout {
            flowMaxWidth = 0,
            flowMinWidth = 0,
            flowTrialPlace = flowStart },
        prepare = \pl -> (drawNone, flowStart pl) }

-- | A flowed figure for text that will not be broken unless necessary.
tightText :: Appearance -> String -> Figure Flow
tightText _ [] = empty
tightText appr text = res where
    size = length text
    withPlacement pl = res where
        left = flowLeft pl
        width = flowWidth pl
        (x, y) = flowStart pl
        back = flowBack pl
        rem = left + width - x
        cont accum y text = -- TODO: Y limit
            case splitAt width text of
                (text, []) ->
                    (accum |% drawStr appr (left, y) text,
                    (left + length text, y))
                (text, tail) -> cont
                    (accum |% drawStr appr (left, y) text)
                    (y + 1) tail
        res = case (size <= rem, size <= width) of
            (True, _) ->
                let end = (x + size, y)
                in (end, (drawStr appr (x, y) text, end))
            (False, True) ->
                let end = (left + size, y + 1)
                in (end, (drawSpace back (x, y) rem |%
                    drawStr appr (left, y + 1) text, end))
            (False, False) ->
                let end =
                      (left + mod (x + size - left) width,
                      y + div (x + size - left) width)
                in let (text, tail) = splitAt rem text
                in (end, cont (drawStr appr (x, y) text) (y + 1) tail)
    res = Figure {
        layout = FlowLayout {
            flowMaxWidth = size,
            flowMinWidth = size,
            flowTrialPlace = fst . withPlacement },
        prepare = snd . withPlacement }

-- | A flowed figure for a breaking space of the given size.
space :: Int -> Figure Flow
space 0 = empty
space size = res where
    prepare pl = res where
        (left, width) = (flowLeft pl, flowWidth pl)
        (x, y) = flowStart pl
        back = flowBack pl
        res = case () of -- TODO: Y limit
            _ | x == left -> (drawNone, (x, y))
            _ | x + size >= left + width ->
                (drawSpace back (x, y) (left + width - x), (left, y + 1))
            _ -> (drawSpace back (x, y) size, (x + size, y))
    res = Figure {
        layout = FlowLayout {
            flowMaxWidth = size,
            flowMinWidth = 0,
            flowTrialPlace = snd . prepare },
        prepare = prepare }

-- | @a@ is a type that behaves like a flowed figure.
class FlowLike a where

    -- | Concatenates flowed figures.
    (+++) :: a -> a -> a

instance FlowLike (Figure Flow) where
    (+++) x y | flowMaxWidth (layout x) == 0 = y
    (+++) x y | flowMaxWidth (layout y) == 0 = x
    (+++) x y = Figure {
        layout =
            let (xL, yL) = (layout x, layout y)
            in FlowLayout {
                flowMaxWidth = flowMaxWidth xL + flowMaxWidth yL,
                flowMinWidth = max (flowMinWidth xL) (flowMinWidth yL),
                flowTrialPlace = \pl -> flowTrialPlace yL $
                    pl { flowStart = flowTrialPlace xL pl } },
        prepare = \pl ->
            let (xDraw, yStart) = prepare x pl
            in let (yDraw, end) = prepare y $ pl { flowStart = yStart }
            in (xDraw |% yDraw, end) }

-- | A flowed figure for text, with breaking spaces.
text :: Appearance -> String -> Figure Flow
text appr = breakSpace 0 where
    breakWord a [] = tightText appr (reverse a)
    breakWord a (' ' : xs) = tightText appr (reverse a) +++ breakSpace 1 xs
    breakWord a (x : xs) = breakWord (x : a) xs
    breakSpace n [] = space n
    breakSpace n (' ' : xs) = breakSpace (n + 1) xs
    breakSpace n (x : xs) = space n +++ breakWord [x] xs

-- | Figures of type @a@ can be drawn directly for testing and debugging
-- purposes.
class HasTestDraw a where
    test :: Figure a -> IO ()

instance HasTestDraw Flow where
    test fig = do
        let placement = FlowPlacement {
            flowBack = (Dull, Blue),
            flowLeft = 0,
            flowWidth = 60,
            flowStart = (0, 0),
            flowLimit = 1000 }
        let (draw, (_, endY)) = prepare fig placement
        runDrawInline (endY + 1) draw
