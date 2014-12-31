{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Terminal.Figure.Block (
    Block,
    Dock,
    Dep,
    Ind,
    Fix,
    Vary,
    blockify,
    Border,
    withBorder,
    lineBorder,
    box,
    setWidth,
    setHeight,
    hcenter,
    vcenter,
    center
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
type instance Placement (Block s) = (Placement s, Point)
type instance Bonus (Block s) = (Width, Height)

-- | The layout type for a block figure which takes up a variable-size
-- rectangular area.
type Dock = Block (Ind Vary Vary)

-- | The sizing type for a block where the width and height are interrelated
-- and specifying one will determine the other.
data Dep

type instance Layout Dep = Either Width Height -> (Width, Height)
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

-- | Tests a block figure by specifying a size.
testPlace :: Placement s -> Figure (Block s) -> IO ()
testPlace size fig = do
    let (draw, (_, height)) = place fig (size, (0, 0))
    runDrawInline height draw

instance CanTest (Block Dep) where
    test = testPlace $ Left testWidth
instance CanTest (Block (Ind Fix Fix)) where
    test = testPlace ((), ())
instance CanTest (Block (Ind Vary Fix)) where
    test = testPlace (testWidth, ())
instance CanTest (Block (Ind Fix Vary)) where
    test = testPlace ((), testHeight)
instance CanTest (Block (Ind Vary Vary)) where
    test = testPlace (testWidth, testHeight)

-- | Converts a flowed figure into a dependently-sized block with the given
-- back color.
blockify :: (FigureLike f) => FullColor -> f Flow -> f (Block Dep)
blockify back flow = compose (\eval -> blockify' <$> eval flow) where
    blockify' flow = res where
        flowL = layoutS flow
        width' _ _ = undefined :: Width -- TODO: Binary search
        area width = FlowArea { width = width, indent = 0 }
        l = (\flowL spec -> case spec of
            (Left width) -> (width, 1 + snd (trialPlace flowL $ area width))
            (Right height) -> (width' flowL height, height)) <$> flowL
        res = figureS l $ funS placed
        placed (Left width, offset@(x, y)) = res where
            (flowDraw, flowEnd) = break2S $ placeS flow <*>
                pure (area width, back, offset)
            endSpace = (\(ex, ey) -> Draw.space back
                (x + ex, y + ey) (width - ex)) <$> flowEnd
            height = (+ 1) <$> sndS flowEnd
            res = plex2S (plusS flowDraw endSpace) $ plex2S (pure width) height
        placed (Right height, offset@(x, y)) = res where
            width = width' <$> flowL <*> pure height
            (flowDraw, flowEnd) = break2S $ placeS flow <*>
                ((\width -> (area width, back, offset)) <$> width)
            endSpace = (\width (ex, ey) -> Draw.space back
                (x + ex, y + ey) (width - ex) |%|
                Draw.fill back (x, y + ey + 1) width (height - ey - 1))
                <$> width <*> flowEnd
            res = plex2S (plusS flowDraw endSpace) $ plex2S width (pure height)

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
        f x s = let (w, h) = x s in (w + width, h + height)
        g (Left w) = Left (w - width)
        g (Right h) = Right (h - height)

-- | Describes a border that can be applied to a block. The size of the border
-- in each direction (left, top, right, bottom) is given, along with a function
-- which draws the border given the size and placement of full block (including
-- border).
type Border =
    (Width, Height, Width, Height,
    (Width, Height) -> Point -> Draw)

-- | Applies a border to a block.
withBorder :: forall s n f. (FigureLike f, CanEnclose s n)
    => Border -> f (Block s) -> f (Block n)
withBorder border block = compose (\eval -> withBorder' <$> eval block) where
    withBorder' block = res where
        (left, top, right, bottom, drawBorder') = border
        padWidth = left + right
        padHeight = top + bottom
        (f, g) = transEnclose (undefined :: s) (padWidth, padHeight)
        res = figureS (f <$> layoutS block) $ funS placed
        placed (size, (x, y)) = res where
            (drawInner, innerSize) = break2S $ placeS block <*>
                pure (g size, (x + left, y + top))
            fullSize = (\(w, h) -> (w + padWidth, h + padHeight)) <$> innerSize
            drawBorder = (\size -> drawBorder' size (x, y)) <$> fullSize
            res = plex2S (plusS drawInner drawBorder) fullSize

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
box :: (FigureLike f, CanEnclose s n) => Appearance
    -> f (Block s) -> f (Block n)
box = withBorder . lineBorder

-- | @s@ is a sizing type for a block for which the size of the axis specified
-- by @p@ can be set, resulting in a block of sizing type @n@.
class CanSetSize p s n | p s -> n where

    -- | Sets the size of an axis of a block.
    setSize :: (FigureLike f) => p -> f (Block s) -> f (Block n)

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
instance CanDepSetSize p => CanSetSize p Dep (Ind Fix Fix) where
    setSize size block = compose (\eval -> setSize' <$> eval block) where
        setSize' block = res where
            place = pure $ depPlace size
            size' = layoutS block <*> place
            res = figureS size' $ funS $ \(_, offset) ->
                (placeS block <*> plex2S place (pure offset))

-- | Sets the width of a block.
setWidth :: (FigureLike f, CanSetSize H s n)
    => Width -> f (Block s) -> f (Block n)
setWidth width = setSize (H width)

-- | Sets the height of a block.
setHeight :: (FigureLike f, CanSetSize V s n)
    => Height -> f (Block s) -> f (Block n)
setHeight height = setSize (V height)

-- | @s@ is the sizing type for a block which can be centered about the axis
-- specified by @p@, resulting in a block of sizing type @n@.
class CanCenter p s n | p s -> n where

    -- | Centers a block along the given axis.
    centerAxis :: (FigureLike f) => p -> f (Block s) -> f (Block n)

instance CanCenter H (Ind Fix a) (Ind Vary a) where
    centerAxis _ block = compose (\eval -> centerAxis' <$> eval block) where
        centerAxis' block = res where
            layout = (\(_, v) -> ((), v)) <$> layoutS block
            res = figureS layout $ funS placed
            placed ((fw, v), (x, y)) = res where
                (draw, size) = break2S $ placeS block <*>
                    ((\(iw, _) -> (((), v), (x + (fw - iw) `div` 2, y))) <$>
                    layoutS block)
                res = plex2S draw $ plex2S (pure fw) (sndS size)
instance CanCenter V (Ind a Fix) (Ind a Vary) where
    centerAxis _ block = compose (\eval -> centerAxis' <$> eval block) where
        centerAxis' block = res where
            layout = (\(h, _) -> (h, ())) <$> layoutS block
            res = figureS layout $ funS placed
            placed ((h, fh), (x, y)) = res where
                (draw, size) = break2S $ placeS block <*>
                    ((\(_, ih) -> ((h, ()), (x, y + (fh - ih) `div` 2))) <$>
                    layoutS block)
                res = plex2S draw $ plex2S (fstS size) (pure fh)

-- | Centers a block along the horizontal axis.
hcenter :: (FigureLike f, CanCenter H s n) => f (Block s) -> f (Block n)
hcenter = centerAxis (undefined :: H)

-- | Centers a block along the vertical axis.
vcenter :: (FigureLike f, CanCenter V s n) => f (Block s) -> f (Block n)
vcenter = centerAxis (undefined :: V)

-- | Centers a block along both axes, using the given color for filler.
center :: (FigureLike f, CanCenter H s m, CanCenter V m n)
    => f (Block s) -> f (Block n)
center = vcenter . hcenter
