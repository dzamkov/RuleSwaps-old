{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Terminal.Figure (
    Layout (..),
    Placement (..),
    Flow,
    Block,
    Fix,
    Vary,
    Dock,
    HasBlockWidth (..),
    HasBlockHeight (..),
    Bonus,
    Figure (..),
    tightText,
    space,
    FlowLike ((+++)),
    text,
    setWidth,
    setHeight,
    test
) where

import System.Console.ANSI
import Terminal.Draw hiding (space)
import qualified Terminal.Draw as Draw

-- | Describes the sizing information contained with a figure with the given
-- mark.
data family Layout a

-- | Describes the placement information needed to draw a figure with the given
-- mark.
data family Placement a

-- | Contains potentially useful information that was computed during the
-- drawing of a figure.
type family Bonus a

-- | Marks a figure as flowed, that is, linear and horizontal with the ability
-- to be broken up into parts, much like text.
data Flow

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

-- | Contains the placement information for a flowed figure.
data instance Placement Flow = FlowPlacement {

    -- | The color used to fill empty space in the flow (spaces and breaks).
    flowBack :: CompleteColor,

    -- | The horizontal offset of the flow area.
    flowLeft :: X,

    -- | The width of the flow area.
    flowWidth :: Width,

    -- | The initial position of the cursor in the flow area.
    flowStart :: Point }

-- | The point at which the remainder of the flow starts.
type instance Bonus Flow = Point

-- | Marks a figure as a block, taking up a rectangular area where the size
-- along the horizontal and vertical axes can independently be fixed or
-- variable.
data Block h v

-- | Marks a block axis as fixed-size.
data Fix

-- | Marks a block axis as variable-size.
data Vary

-- | Marks a figure as a docked block, taking up a variable-size rectangular
-- area.
type Dock = Block Vary Vary

-- | Contains sizing information for a block figure.
data instance Layout (Block h v) = BlockLayout {

    -- | Contains sizing information for the horizontal axis of this block
    -- figure.
    blockLayoutHorizontal :: Layout h,

    -- | Contains sizing information for the vertical axis of this block
    -- figure.
    blockLayoutVertical :: Layout v }

-- | Contains placement information for a block figure.
data instance Placement (Block h v) = BlockPlacement {

    -- | The position of top-left corner the block.
    blockOffset :: Point,

    -- | Contains placement information for the horizontal axis of this block
    -- figure.
    blockPlacementHorizontal :: Placement h,

    -- | Contains placement information for the vertical axis of this block
    -- figure.
    blockPlacementVertical :: Placement v }

-- | Contains layout information for a fixed-size axis.
data instance Layout Fix = FixLayout Width

-- | Contains placement information for a fixed-size axis.
data instance Placement Fix = FixPlacement

-- | Contains layout information for a variable-size axis.
data instance Layout Vary = VaryLayout

-- | Contains placement information for a variable-size axis.
data instance Placement Vary = VaryPlacement Width

-- | @a@ is the type for a layout or placement that has a block width.
class HasBlockWidth a where

    -- | The width of this block.
    blockWidth :: a -> Width

-- | @a@ is the type for a layout or placement that has a block height.
class HasBlockHeight a where

    -- | The height of this block.
    blockHeight :: a -> Height

instance HasBlockWidth (Layout (Block Fix a)) where
    blockWidth layout =
        let (FixLayout width) = blockLayoutHorizontal layout
        in width
instance HasBlockWidth (Placement (Block Vary a)) where
    blockWidth placement =
        let (VaryPlacement width) = blockPlacementHorizontal placement
        in width
instance HasBlockHeight (Layout (Block a Fix)) where
    blockHeight layout =
        let (FixLayout height) = blockLayoutVertical layout
        in height
instance HasBlockHeight (Placement (Block a Vary)) where
    blockHeight placement =
        let (VaryPlacement height) = blockPlacementVertical placement
        in height

-- | Blocks have no bonus information.
type instance Bonus (Block h v) = ()

-- | A static figure that can be drawn to a terminal.
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
        prepare = \pl -> (none, flowStart pl) }

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
                    (accum |% Draw.string appr (left, y) text,
                    (left + length text, y))
                (text, tail) -> cont
                    (accum |% Draw.string appr (left, y) text)
                    (y + 1) tail
        res = case (size <= rem, size <= width) of
            (True, _) ->
                let end = (x + size, y)
                in (end, (Draw.string appr (x, y) text, end))
            (False, True) ->
                let end = (left + size, y + 1)
                in (end, (Draw.space back (x, y) rem |%
                    Draw.string appr (left, y + 1) text, end))
            (False, False) ->
                let end =
                      (left + mod (x + size - left) width,
                      y + div (x + size - left) width)
                in let (text, tail) = splitAt rem text
                in (end, cont (Draw.string appr (x, y) text) (y + 1) tail)
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
        res = case () of
            _ | x == left -> (Draw.none, (x, y))
            _ | x + size >= left + width ->
                (Draw.space back (x, y) (left + width - x), (left, y + 1))
            _ -> (Draw.space back (x, y) size, (x + size, y))
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

-- | Sets the width of a block with a variable width.
setWidth :: Width -> Figure (Block Vary a) -> Figure (Block Fix a)
setWidth width fig = Figure {
    layout = BlockLayout {
        blockLayoutHorizontal = FixLayout width,
        blockLayoutVertical = blockLayoutVertical $ layout fig },
    prepare = \placement -> prepare fig BlockPlacement {
        blockOffset = blockOffset placement,
        blockPlacementHorizontal = VaryPlacement width,
        blockPlacementVertical = blockPlacementVertical placement } }

-- | Sets the height of a block with a variable width.
setHeight :: Height -> Figure (Block a Vary) -> Figure (Block a Fix)
setHeight height fig = Figure {
    layout = BlockLayout {
        blockLayoutHorizontal = blockLayoutHorizontal $ layout fig,
        blockLayoutVertical = FixLayout height },
    prepare = \placement -> prepare fig BlockPlacement {
        blockOffset = blockOffset placement,
        blockPlacementHorizontal = blockPlacementHorizontal placement,
        blockPlacementVertical = VaryPlacement height } }

-- | Figures of type @a@ can be drawn directly for testing and debugging
-- purposes.
class HasTestDraw a where
    test :: Figure a -> IO ()

-- | The width of variable-width figures drawn using 'test'.
testWidth :: Width
testWidth = 60

-- | The height of variable-height figures drawn using 'test'.
testHeight :: Height
testHeight = 30

instance HasTestDraw Flow where
    test fig = do
        let placement = FlowPlacement {
            flowBack = (Dull, Blue),
            flowLeft = 0,
            flowWidth = testWidth,
            flowStart = (0, 0) }
        let (draw, (_, endY)) = prepare fig placement
        runDrawInline (endY + 1) draw
instance HasTestDraw (Block Vary Vary) where
    test = test . setWidth testWidth
instance HasTestDraw (Block Vary Fix) where
    test = test . setWidth testWidth
instance HasTestDraw (Block Fix Vary) where
    test = test . setHeight testHeight
instance HasTestDraw (Block Fix Fix) where
    test fig = do
        let placement = BlockPlacement {
            blockOffset = (0, 0),
            blockPlacementHorizontal = FixPlacement,
            blockPlacementVertical = FixPlacement }
        let (draw, _) = prepare fig placement
        runDrawInline (blockHeight $ layout fig) draw
