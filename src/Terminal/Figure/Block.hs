{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Terminal.Figure.Block (
    Block,
    Dock,
    Dep,
    Ind,
    Fix,
    Vary,
    SBlock,
    blockify,
    Border,
    withBorder,
    lineBorder,
    box,
    setWidth,
    setHeight
) where

import Stride hiding (end)
import Terminal.Draw
import qualified Terminal.Draw as Draw
import Terminal.Figure.Core
import Terminal.Figure.Flow
import Control.Applicative hiding (empty)

-- | The layout type for a block figure, one which takes up a rectangular area.
-- The @s@ specifies how sizing is performed for the block.
data Block s

type instance Layout (Block s) = Layout s
type instance Placement (Block s) = Placement s
type instance Next (Block s) = SBlock

-- | The layout type for a block figure which takes up a variable-size
-- rectangular area.
type Dock = Block (Ind Vary Vary)

-- | The sizing type for a block where the width and height are interrelated
-- and specifying one will determine the other.
data Dep

type instance Layout Dep = ()
type instance Placement Dep = Either Width Height

-- | The sizing type for a block where the width and height are independently
-- specified or determined.
data Ind (h :: * -> *) (v :: * -> *)

type instance Layout (Ind h v) = (Layout (h H), Layout (v V))
type instance Placement (Ind h v) = (Placement (h H), Placement (v V))

-- | Specifies the horizontal axis.
newtype H = H Width

-- | Specifies the vertical axis.
newtype V = V Height

-- | Specifies an axis to have a determined size.
data Fix :: * -> *

type instance Layout (Fix H) = Width
type instance Layout (Fix V) = Height
type instance Placement (Fix a) = ()

-- | Specifies an axis to have a variable size.
data Vary :: * -> *

type instance Layout (Vary a) = ()
type instance Placement (Vary H) = Width
type instance Placement (Vary V) = Height

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
instance CanTest (Block (Ind Fix Fix)) where
    test fig = test $ place fig ((), ())
instance CanTest (Block (Ind Vary Fix)) where
    test fig = test $ place fig (testWidth, ())
instance CanTest (Block (Ind Fix Vary)) where
    test fig = test $ place fig ((), testHeight)
instance CanTest (Block (Ind Vary Vary)) where
    test fig = test $ place fig (testWidth, testHeight)

-- | @a@ is a type similar to a flowed figure which can be converted into
-- a dependently-sized block of type @b@.
class CanBlockify a b | a -> b where

    -- | Converts a flowed figure into a dependently-sized block using the
    -- given back color.
    blockify :: FullColor -> a -> b

instance CanBlockify (Stride (Figure Flow)) (Stride (Figure (Block Dep))) where
    blockify back flow = res where
        res = dfigure (pure ()) withSize
        withSize (Left width) = res where
            flowP = dplace flow $ pure FlowArea {
                width = width,
                indent = 0 }
            end' = end <$> dlayout flowP
            endY = dsnd end'
            size = dcheck $ dplex2 (pure width) ((+ 1) <$> endY)
            res = dfigure size (withOffset width flowP end')
        withSize (Right _) = undefined -- TODO: Math and binary search
        withOffset width flowP end (x, y) = res where
            flowD = ddraw $ dplace flowP $ pure (back, (x, y))
            endSpace = (\(ex, ey) -> Draw.space back
                (x + ex, y + ey) (width - ex)) <$> end
            res = dstatic $ dplus flowD endSpace
instance CanBlockify (Figure Flow) (Figure (Block Dep)) where
    blockify back flow = start $ blockify back $ stay flow

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

-- | Describes a border that can be applied to a block. The size of the border
-- in each direction (left, top, right, bottom) is given, along with a function
-- which draws the border given the size and placement of full block (including
-- border).
type Border =
    (Width, Height, Width, Height,
    (Width, Height) -> Point -> Draw)

-- | @a@ is a type similar to a block to which a border can be applied.
class CanBorder a where

    -- | Applies a border to a block.
    withBorder :: Border -> a -> a

instance CanEnclose s s => CanBorder (Stride (Figure (Block s))) where
    withBorder border block = res where
        (left, top, right, bottom, drawBorder) = border
        padWidth = left + right
        padHeight = top + bottom
        (f, g) = transEnclose (undefined :: s) (padWidth, padHeight)
        res = dfigure (f <$> dlayout block) withSize
        withSize size = res where
            blockP = dplace block $ pure (g size)
            innerSize = dlayout blockP
            fullSize = (\(w, h) -> (w + padWidth, h + padHeight)) <$> innerSize
            res = dfigure fullSize withOffset
            withOffset (x, y) = res where
                blockD = ddraw $ dplace blockP $ pure (x + 1, y + 1)
                borderD = (\size -> drawBorder size (x, y)) <$> fullSize
                res = dstatic $ dplus blockD borderD
instance CanEnclose s s => CanBorder (Figure (Block s)) where
    withBorder border block = start $ withBorder border $ stay block

-- | A 1-character-thick line border with the given appearance.
lineBorder :: Appearance -> Border
lineBorder appr = (1, 1, 1, 1, draw) where
    draw (width, height) (x, y) = foldl1 (|%|) [
        Draw.string appr (x, y) "+",
        Draw.hline appr '-' (x + 1, y) (width - 2),
        Draw.string appr (x + width - 1, y) "+",
        Draw.vline appr '|' (x + width - 1, y + 1) (height - 2),
        Draw.string appr (x + width - 1, y + height - 1) "+",
        Draw.hline appr '-' (x + 1, y + height - 1) (width - 2),
        Draw.string appr (x, y + height - 1) "+",
        Draw.vline appr '|' (x, y + 1) (height - 2)]

-- | Encloses a block with a graphical box of the given appearance.
box :: (CanBorder a) => Appearance -> a -> a
box = withBorder . lineBorder

-- | @a@ is a type similar to a block for which the size of the axis specified
-- by @p@ can be set, resulting in a block of type @b@.
class CanSetSize p a b | p a -> b where

    -- | Sets the size of an axis of a block.
    setSize :: p -> a -> b

-- | @p@ is an axis that can be set to a given size within a dependently-sized
-- block.
class CanDepSetSize p where

    -- | Gets the placement for a dependently-sized block when the given size
    -- is given to an axis.
    depPlace :: p -> Placement Dep

instance CanDepSetSize H where
    depPlace (H width) = Left width
instance CanDepSetSize V where
    depPlace (V height) = Right height
instance CanDepSetSize p => CanSetSize p
    (Stride (Figure (Block Dep)))
    (Stride (Figure (Block (Ind Fix Fix)))) where
        setSize size block = res where
            blockP = dplace block $ pure $ depPlace size
            res = dfigure (dlayout blockP) $ const blockP
instance CanSetSize p (Stride (Figure (Block a))) (Stride (Figure (Block b)))
    => CanSetSize p (Figure (Block a)) (Figure (Block b)) where
        setSize size block = start $ setSize size $ stay block

-- | Sets the width of a block.
setWidth :: (CanSetSize H a b) => Width -> a -> b
setWidth width = setSize (H width)

-- | Sets the height of a block.
setHeight :: (CanSetSize V a b) => Height -> a -> b
setHeight height = setSize (V height)
