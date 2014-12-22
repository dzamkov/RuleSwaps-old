{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Terminal.Figure.Block (
    Block,
    SizeSpecifier,
    Dep,
    SBlock,
    blockify
) where

import Terminal.Draw
import qualified Terminal.Draw as Draw
import Terminal.Figure.Core
import Terminal.Figure.Flow

-- | The layot type for a block figure, one which takes up a rectangular area.
-- The @s@ specifies how sizing is performed for the block.
data Block s

-- | The information needed to fully determine the size of a block with sizing
-- type @s@.
type family SizeSpecifier s

type instance Layout (Block s) = Layout s
type instance Placement (Block s) = To (SizeSpecifier s) SBlock

-- | The sizing type for a block where the width and height are interrelated
-- and specifying one will determine the other.
data Dep

type instance Layout Dep = ()
type instance SizeSpecifier Dep = Either Width Height

-- | The layout type for a block figure with a known size.
data SBlock

type instance Layout SBlock = (Width, Height)
type instance Placement SBlock = To Point Static

instance HasEmpty SBlock where
    empty = Figure {
        layout = (0, 0),
        place = \(To _) -> empty }
instance CanTest SBlock where
    test fig = do
        let (_, height) = layout fig
        runDrawInline height $ draw $ place fig $ To (0, 0)
instance CanTest (Block Dep) where
    test fig = test $ place fig $ To (Left testWidth)

-- | Converts a figure into a dependently-sized block.
blockify :: FullColor -> Figure Flow -> Figure (Block Dep)
blockify back flow = res where
    res = Figure {
        layout = (),
        place = \(To size) -> withSize size }
    withSize :: SizeSpecifier Dep -> Figure SBlock
    withSize (Left width) = res where
        pFlow = place flow $ To FlowArea {
            width = width,
            indent = 0 }
        (endX, endY) = end $ layout pFlow
        res :: Figure SBlock
        res = Figure {
            layout = (width, endY + 1),
            place = \(To offset) -> withOffset offset }
        withOffset (x, y) = res where
            pDraw = draw $ place pFlow $ To (back, (x, y))
            endSpace = Draw.space back (x + endX, y + endY) (width - endX)
            res = static (pDraw |% endSpace)
    withSize (Right _) = undefined -- TODO: Math and binary search stuff
