{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Terminal.Block (
    Opacity (..),
    Block (..)
) where

import qualified Markup
import qualified Terminal.Draw as Draw
import Terminal.Context
import Terminal.Paint
import Terminal.Flow (Flow)
import qualified Terminal.Flow as Flow
import Data.Ratio
import Control.Applicative

-- | Specifies the opacity of a block.
data Opacity

    -- | The block contains no translucent parts.
    = Opaque

    -- | The block contains transparent parts that may be given a color.
    | Translucent

    -- | The block has transparent parts (such as those around text) that
    -- /must/ be given a background color.
    | Transparent
    deriving (Eq, Ord, Show)

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

-- | Multiples an integer by a rational.
imult :: (Integral a) => Rational -> a -> a
imult x y = (y * fromInteger (numerator x)) `div` fromInteger (denominator x)

-- | Combines the opacities for non-overlapping blocks.
comps :: Opacity -> Opacity -> Opacity
comps Transparent _ = Transparent
comps _ Transparent = Transparent
comps Translucent _ = Translucent
comps _ Translucent = Translucent
comps Opaque Opaque = Opaque

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

-- | Translates a block produced by the given paint function.
paintTrans :: (Applicative f) => f Offset
    -> (f (Maybe Color, Point) -> Paint f)
    -> f (Maybe Color, Point) -> Paint f
paintTrans offset source context = source $
    (\(ox, oy) (back, (x, y)) -> (back, (x + ox, y + oy)))
    <$> offset <*> context

instance (Applicative f) => Markup.Block Terminal (Block f) where
    clear = Block {
        freeWidth = 1,
        freeHeight = 1,
        opacity = Translucent,
        place = \w h -> (pure 0, pure 0, paintEmpty w h) }
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
    over high _ | opacity high == Opaque = high
    over high low | opacity high == Transparent =
        Markup.over (Markup.setBack (fst defaultAppearance) high) low
    over high low = Block {
        freeWidth = min (freeWidth high) (freeWidth low),
        freeHeight = min (freeHeight high) (freeHeight low),
        opacity = opacity low,
        place = \w h ->
            let (hmw, hmh, hPaint) = place high w h
                (lmw, lmh, lPaint) = place low w h
                mw = max <$> hmw <*> lmw
                mh = max <$> hmh <*> lmh
            in (mw, mh, \c -> hPaint c `over` lPaint c) }
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
    setBack nBack block = block {
        opacity = Opaque,
        place = \w h ->
            let (mw, mh, paint) = place block w h
                nPaint c =
                    let nC = (\(_, offset) -> (Just nBack, offset)) <$> c
                    in paint nC
            in (mw, mh, nPaint) }

instance (Applicative f) => Markup.FlowToBlock
    Terminal (Flow f) (Block f) where
        blockify alignment' flow =
            let alignment = Flow.fromAlignment alignment'
            in Block {
                freeWidth = 1,
                freeHeight = 1,
                opacity = Transparent,
                place = \w h ->
                    let (mh, paintFlow) = Flow.place flow alignment w
                        mw = Flow.minWidth flow
                        eh = (-) <$> h <*> mh
                        nPaint c =
                            paintFlow ((\(Just b, p) -> (b, p)) <$> c) `mix`
                            paintTrans ((\y -> (0, y)) <$> mh)
                                (paintEmpty w eh) c
                    in (mw, mh, nPaint) }
