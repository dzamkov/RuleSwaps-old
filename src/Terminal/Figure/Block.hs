{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Terminal.Figure.Block (
    Block,
    Dep,
    SBlock,
    blockify,
    box
) where

import Terminal.Draw
import qualified Terminal.Draw as Draw
import Terminal.Figure.Core
import Terminal.Figure.Flow

-- | The layot type for a block figure, one which takes up a rectangular area.
-- The @s@ specifies how sizing is performed for the block.
data Block s

type instance Layout (Block s) = Layout s
type instance Placement (Block s) = Placement s
type instance Next (Block s) = SBlock

-- | The sizing type for a block where the width and height are interrelated
-- and specifying one will determine the other.
data Dep

type instance Layout Dep = ()
type instance Placement Dep = Either Width Height

-- | The layout type for a block figure with a known size.
data SBlock

type instance Layout SBlock = (Width, Height)
type instance Placement SBlock = Point
type instance Next SBlock = Static

instance HasEmpty SBlock where
    empty = Figure {
        layout = (0, 0),
        place = const empty }
instance CanTest SBlock where
    test fig = do
        let (_, height) = layout fig
        runDrawInline height $ draw $ place fig (0, 0)
instance CanTest (Block Dep) where
    test fig = test $ place fig $ Left testWidth

-- | Converts a figure into a dependently-sized block.
blockify :: FullColor -> Figure Flow -> Figure (Block Dep)
blockify back flow = res where
    res = Figure {
        layout = (),
        place = withSize }
    withSize :: Placement Dep -> Figure SBlock
    withSize (Left width) = res where
        pFlow = place flow FlowArea {
            width = width,
            indent = 0 }
        (endX, endY) = end $ layout pFlow
        res :: Figure SBlock
        res = Figure {
            layout = (width, endY + 1),
            place = withOffset }
        withOffset (x, y) = res where
            pDraw = draw $ place pFlow (back, (x, y))
            endSpace = Draw.space back (x + endX, y + endY) (width - endX)
            res = static (pDraw |% endSpace)
    withSize (Right _) = undefined -- TODO: Math and binary search stuff

-- | Blocks of sizing type @s@ can be enclosed by constant- width and height
-- borders resulting in blocks of sizing type @n@.
class CanEnclose s n | s -> n where

    -- | Given the total size of the border enclosing a block, gets the
    -- appropriate mappings for layout and size between the inner block and
    -- outer block.
    transEnclose :: s -> (Width, Height)
        -> (Layout s -> Layout n,
            Placement n -> Placement s)

instance CanEnclose Dep Dep where
    transEnclose _ (width, height) = (f, g) where
        f () = ()
        g (Left w) = Left (w - width)
        g (Right h) = Right (h - height)

-- | Encloses a block with a graphical box
box :: forall s n. (CanEnclose s n)
    => Appearance -> Figure (Block s) -> Figure (Block n)
box appr block = res where
    (f, g) = transEnclose (undefined :: s) (2, 2)
    res = Figure {
        layout = f $ layout block,
        place = withSize }
    withSize size = res where
        sBlock = place block (g size)
        (iWidth, iHeight) = layout sBlock
        width = iWidth + 2
        height = iHeight + 2
        res :: Figure SBlock
        res = Figure {
            layout = (width, height),
            place = withOffset }
        withOffset (x, y) = res where
            sDraw = draw $ place sBlock (x + 1, y + 1)
            res = static (sDraw |%
                Draw.string appr (x, y) "+" |%
                Draw.hline appr '-' (x + 1, y) (width - 2) |%
                Draw.string appr (x + width - 1, y) "+" |%
                Draw.vline appr '|' (x + width - 1, y + 1) (height - 2) |%
                Draw.string appr (x + width - 1, y + height - 1) "+" |%
                Draw.hline appr '-' (x + 1, y + height - 1) (width - 2) |%
                Draw.string appr (x, y + height - 1) "+" |%
                Draw.vline appr '|' (x, y + 1) (height - 2))
