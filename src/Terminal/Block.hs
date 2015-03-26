{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Terminal.Block (
    Opacity (..),
    Block (..),
    BorderStyle
) where

import Markup ((===), (|||))
import qualified Markup
import qualified Markup.Attr as Attr
import qualified Terminal.Draw as Draw
import Terminal.Base
import Terminal.Paint
import Terminal.Flow (Flow)
import qualified Terminal.Flow as Flow
import Data.Ratio
import Data.Maybe (fromMaybe)
import Control.Arrow (first)
import Control.Applicative

-- | Specifies the opacity and high-level kind of a block.
data Opacity

    -- | The block contains no translucent parts, and may be of a single
    -- solid color.
    = Opaque (Maybe Color)

    -- | The block contains transparent parts that may be given a color.
    -- If 'True', then the block is entirely transparent.
    | Translucent Bool

    -- | The block has transparent parts (such as those around text) that
    -- /must/ be given a background color.
    | Dependent

-- | A figure, based in the terminal, which takes up a rectangular region.
data Block f = Block {

    -- | Specifies, relative to other blocks, how much free width should be
    -- allocated to this block.
    freeWidth :: Rational,

    -- | Specifies, relative to other blocks, how much free height should be
    -- allocated to this block.
    freeHeight :: Rational,

    -- | Specifies the opacity of the block.
    opacity :: Opacity,

    -- | Places the block by providing a size. This returns the minimum width
    -- and height of the block, along with a function which draws it with the
    -- given background and absolute offset. The block will draw properly as
    -- long as the given size is larger than the minimum size.
    place :: f Width -> f Height
        -> (f Width, f Height,
            f (Maybe Color, Point) -> Paint f) }

instance (Applicative f) => Markup.Block (Block f) where
    (|||) left right =
        let rFreeWidth = freeWidth left + freeWidth right
        in Block {
            freeWidth = rFreeWidth,
            freeHeight = min (freeHeight left) (freeHeight right),
            opacity = comps (opacity left) (opacity right),
            place = \w h ->
                let (lmw, lmh, lPaint) = place left lw h
                    (rmw, rmh, rPaint) = place right rw h
                    mw = (+) <$> lmw <*> rmw
                    mh = max <$> lmh <*> rmh
                    ew = (-) <$> w <*> mw
                    lew = if rFreeWidth == 0 then (`div` 2) <$> ew
                        else imult (freeWidth left / rFreeWidth) <$> ew
                    lw = (+) <$> lew <*> lmw
                    rew = (-) <$> ew <*> lew
                    rw = (+) <$> rew <*> rmw
                in (mw, mh, \c -> lPaint c `mix`
                    paintTrans ((\x -> (x, 0)) <$> lw) rPaint c) }
    (===) top bottom =
        let rFreeHeight = freeHeight top + freeHeight bottom
        in Block {
            freeWidth = min (freeWidth top) (freeWidth bottom),
            freeHeight = rFreeHeight,
            opacity = comps (opacity top) (opacity bottom),
            place = \w h ->
                let (tmw, tmh, tPaint) = place top w th
                    (bmw, bmh, bPaint) = place bottom w bh
                    mw = max <$> tmw <*> bmw
                    mh = (+) <$> tmh <*> bmh
                    eh = (-) <$> h <*> mh
                    teh = if rFreeHeight == 0 then (`div` 2) <$> eh
                        else imult (freeHeight top / rFreeHeight) <$> eh
                    th = (+) <$> teh <*> tmh
                    beh = (-) <$> eh <*> teh
                    bh = (+) <$> beh <*> bmh
                in (mw, mh, \c -> tPaint c `mix`
                    paintTrans ((\y -> (0, y)) <$> th) bPaint c) }
instance (Applicative f) => Markup.BlockSize Width Height (Block f) where
    setWidth _ block | freeWidth block == 0 = block
    setWidth width block = block {
        freeWidth = 0,
        place = \w h ->
            let (mw', mh, paint) = place block w h
                mw = max width <$> mw'
            in (mw, mh, paint) }
    setHeight _ block | freeHeight block == 0 = block
    setHeight height block = block {
        freeHeight = 0,
        place = \w h ->
            let (mw, mh', paint) = place block w h
                mh = max height <$> mh'
            in (mw, mh, paint) }
instance (Applicative f) => Markup.BlockSolid Color (Block f) where
    solid color = Block {
        freeWidth = 1,
        freeHeight = 1,
        opacity = Opaque (Just color),
        place = \w h -> (pure 0, pure 0, paintSolid (pure color) w h) }
instance (Applicative f) => Markup.BlockTrans (Block f) where
    clear = Block {
        freeWidth = 1,
        freeHeight = 1,
        opacity = Translucent True,
        place = \w h -> (pure 0, pure 0, paintEmpty w h) }
    over high low = res where
        setBack nBack nOpacity = high {
            opacity = nOpacity,
            place = \w h ->
                let (mw, mh, paint) = place high w h
                    nPaint c =
                        let nC = (\(_, offset) -> (Just nBack, offset)) <$> c
                        in paint nC
                in (mw, mh, nPaint) }
        res = case (opacity high, opacity low) of
            (Opaque _, _) -> high
            (_, Translucent True) -> high
            (Translucent True, _) -> low
            (_, Opaque (Just col)) -> setBack col (Opaque Nothing)
            (Dependent, _) -> setBack (fst defaultAppearance) (Opaque Nothing)
            (_, lowOpacity) -> Block {
                freeWidth = min (freeWidth high) (freeWidth low),
                freeHeight = min (freeHeight high) (freeHeight low),
                opacity = lowOpacity,
                place = \w h ->
                    let (hmw, hmh, hPaint) = place high w h
                        (lmw, lmh, lPaint) = place low w h
                        mw = max <$> hmw <*> lmw
                        mh = max <$> hmh <*> lmh
                    in (mw, mh, \c -> hPaint c `over` lPaint c) }
instance (Applicative f) => Markup.BlockBorder BorderStyle (Block f) where
    withBorder style' block =
        let style = style' Attr.defaultStyle
            (l', t', r', b') = borderMargin style
            base = Markup.solid $ borderColor style
            l = Markup.setWidth l' base
            t = Markup.setHeight t' base
            r = Markup.setWidth r' base
            b = Markup.setHeight b' base
        in t === l ||| block ||| r === b
instance (Applicative f) => Markup.FlowToBlock (Flow f) (Block f) where
    blockify alignment' flow =
        let alignment = Flow.fromAlignment alignment'
        in Block {
            freeWidth = 1,
            freeHeight = 1,
            opacity = Dependent,
            place = \w h ->
                let getBack = fromMaybe $
                        error "back color must be provided"
                    (mh, paintFlow) = Flow.place flow alignment w
                    mw = Flow.minWidth flow
                    eh = (-) <$> h <*> mh
                    nPaint c =
                        paintFlow (first getBack <$> c) `mix`
                        paintTrans ((\y -> (0, y)) <$> mh)
                            (paintEmpty w eh) c
                in (mw, mh, nPaint) }

-- | Multiples an integer by a rational.
imult :: (Integral a) => Rational -> a -> a
imult x y = (y * fromInteger (numerator x)) `div` fromInteger (denominator x)

-- | Combines the opacities for non-overlapping blocks.
comps :: Opacity -> Opacity -> Opacity
comps Dependent _ = Dependent
comps _ Dependent = Dependent
comps (Translucent True) (Translucent True) = Translucent True
comps (Translucent _) _ = Translucent False
comps _ (Translucent _) = Translucent False
comps (Opaque (Just x)) (Opaque (Just y)) | x == y = Opaque (Just x)
comps (Opaque _) (Opaque _) = Opaque Nothing

-- | Constructs a paint function which paints a translucent block of the given
-- width and height.
paintEmpty :: (Applicative f)
    => f Width -> f Height
    -> f (Maybe Color, Point) -> Paint f
paintEmpty width height context = toPaint $
    (\w h (back, point) -> case back of
        Nothing -> Draw.none
        Just back -> Draw.fill back point w h)
    <$> width <*> height <*> context

-- | Constructs a paint function which paints a solid block of the given
-- width, height and color/
paintSolid :: (Applicative f)
    => f Color -> f Width -> f Height
    -> f (Maybe Color, Point) -> Paint f
paintSolid color width height context = toPaint $
    (\color w h (_, point) -> Draw.fill color point w h)
    <$> color <*> width <*> height <*> context

-- | Translates a block produced by the given paint function.
paintTrans :: (Applicative f) => f Offset
    -> (f (Maybe Color, Point) -> Paint f)
    -> f (Maybe Color, Point) -> Paint f
paintTrans offset source context = source $
    (\(ox, oy) (back, (x, y)) -> (back, (x + ox, y + oy)))
    <$> offset <*> context

-- | Describes the style of block border.
data BorderStyle = BorderStyle {

    -- | The margin for the border.
    borderMargin :: (Width, Height, Width, Height),

    -- | The color of the border.
    borderColor :: Color }

instance Attr.AttrColor Color BorderStyle where
    color c style = style { borderColor = c }
instance Attr.AttrMargin Width Height BorderStyle where
    margin l t r b style = style { borderMargin = (l, t, r, b) }
instance Attr.HasDefault BorderStyle where
    defaultStyle = BorderStyle {
        borderMargin = (1, 1, 1, 1),
        borderColor = snd defaultAppearance }
