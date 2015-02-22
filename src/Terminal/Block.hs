{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Terminal.Block (
    Size (..),
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

-- | Specifies how the length of a block along an axis is determined. @a@ is a
-- concrete length along the axis.
data Size a

    -- | The axis has a known fixed length.
    = Fix a

    -- | The axis has a variable length which must be at least the given value.
    -- When in a stack, the given non-zero rational is used to weigh how much
    -- free space is given to this block.
    | Vary a Rational

    -- | The axis has a length dependent on the other axis. It is not possible
    -- to specify the length directly.
    | Dep
    deriving (Show)

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

    -- | Specifies how the width of the block is determined.
    width :: Size (f Width),

    -- | Specifies how the height of the block is determined.
    height :: Size (f Height),

    -- | Specifies the opacity of the block.
    opacity :: Opacity,

    -- | Places the block by providing sizing information.
    -- Returns the final width and height of the block, along with a function
    -- which draws it with the given background and absolute offset.
    place :: f Width -> f Height
        -> (f Width, f Height,
            f (Maybe Color, Point) -> Paint f) }

-- | Ensures the size specifiers of the given block are both 'Fix' if the size
-- of the given block is completely determined.
explait :: Block f -> Block f
explait block = case (width block, height block) of
    (Fix width', Dep) ->
        let (width, height, paint) = place block width' undefined
        in block {
            height = Fix height,
            place = \_ _ -> (width, height, paint) }
    (Dep, Fix height') ->
        let (width, height, paint) = place block undefined height'
        in block {
            width = Fix width,
            place = \_ _ -> (width, height, paint) }
    _ -> block

-- | Combines the 'Size's for blocks that are stacked along the axis in
-- question. Also returns a function which distributes length accordingly.
stack :: (Applicative f, Integral a)
    => Size (f a) -> Size (f a)
    -> (Size (f a), f a -> (f a, f a))
stack (Fix xLen) (Fix yLen) =
    (Fix ((+) <$> xLen <*> yLen), const (xLen, yLen))
stack (Fix xLen) (Vary yMinLen weight) =
    (Vary ((+) <$> xLen <*> yMinLen) weight,
        \len -> (xLen, (-) <$> len <*> xLen))
stack (Fix xLen) Dep = (Dep, const (xLen, undefined))
stack (Vary xMinLen weight) (Fix yLen) =
    (Vary ((+) <$> xMinLen <*> yLen) weight,
        \len -> ((-) <$> len <*> yLen, yLen))
stack (Vary xMinLen xWeight) (Vary yMinLen yWeight) =
    let weight = xWeight + yWeight
    in (Vary ((+) <$> xMinLen <*> yMinLen) weight, \len ->
        let fi = fromInteger
            times r i = div (i * fi (numerator r)) (fi $ denominator r)
            eLen = (\l x y -> l - x - y) <$> len <*> xMinLen <*> yMinLen
            xeLen = times (xWeight / weight) <$> eLen
            yeLen = (-) <$> eLen <*> xeLen
        in ((+) <$> xMinLen <*> xeLen,
            (+) <$> yMinLen <*> yeLen))
stack (Vary xMinLen _) Dep = (Dep, const (xMinLen, undefined))
stack Dep (Fix yLen) = (Dep, const (undefined, yLen))
stack Dep (Vary yMinLen _) = (Dep, const (undefined, yMinLen))
stack Dep Dep = (Dep, const (undefined, undefined))

-- | Combines the 'Size's for blocks that are stacked against the axis in
-- question such that both blocks have the same length.
mediate :: (Applicative f, Integral a)
    => Size (f a) -> Size (f a)
    -> (Size (f a), Bool, Bool, f a -> (f a, f a))
mediate (Fix xLen) (Fix yLen) =
    let len = max <$> xLen <*> yLen
    in (Fix len, False, False, const (xLen, yLen))
mediate (Fix xLen) (Vary yMinLen weight) =
    let minLen = max <$> xLen <*> yMinLen
    in (Vary minLen weight, False, True, \len -> (xLen, len))
mediate (Fix xLen) Dep = (Dep, False, False, const (xLen, undefined))
mediate (Vary xMinLen weight) (Fix yLen) =
    let minLen = max <$> xMinLen <*> yLen
    in (Vary minLen weight, True, False, \len -> (len, yLen))
mediate (Vary xMinLen xWeight) (Vary yMinLen yWeight) =
    let minLen = max <$> xMinLen <*> yMinLen
        weight = max xWeight yWeight
    in (Vary minLen weight, True, True, \len -> (len, len))
mediate (Vary xMinLen _) Dep = (Dep, False, False, const (xMinLen, undefined))
mediate Dep (Fix yLen) = (Dep, False, False, const (undefined, yLen))
mediate Dep (Vary yMinLen _) = (Dep, False, False, const (undefined, yMinLen))
mediate Dep Dep = (Dep, False, False, const (undefined, undefined))

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

-- | Adds the given width to a block with the given size and paint function.
paintHPad :: (Applicative f) => f Width
    -> f Width -> f Height
    -> (f (Maybe Color, Point) -> Paint f)
    -> f (Maybe Color, Point) -> Paint f
paintHPad add width height source context = res where
    info = (\add ->
        let left = add `div` 2
            right = add - left
        in (left, right)) <$> add
    res = foldl1 mix [
        paintEmpty (fst <$> info) height context,
        paintTrans ((\(l, _) -> (l, 0)) <$> info) source context,
        paintTrans ((\(l, _) w -> (l + w, 0)) <$> info <*> width)
            (paintEmpty (snd <$> info) height) context]

-- | Adds the given height to a block with the given size and paint function.
paintVPad :: (Applicative f) => f Height
    -> f Width -> f Height
    -> (f (Maybe Color, Point) -> Paint f)
    -> f (Maybe Color, Point) -> Paint f
paintVPad add width height source context = res where
    info = (\add ->
        let top = add `div` 2
            bottom = add - top
        in (top, bottom)) <$> add
    res = foldl1 mix [
        paintEmpty width (fst <$> info) context,
        paintTrans ((\(t, _) -> (0, t)) <$> info) source context,
        paintTrans ((\(t, _) h -> (0, t + h)) <$> info <*> height)
            (paintEmpty width (snd <$> info)) context]

instance (Applicative f) => Markup.Block Terminal (Block f) where
    clear = Block {
        width = Vary (pure 0) 1,
        height = Vary (pure 0) 1,
        opacity = Translucent,
        place = \w h -> (w, h, paintEmpty w h) }
    (|||) left right = res where
        (resWidth, wDist) = stack (width left) (width right)
        (resHeight, lM, rM, hDist) = mediate (height left) (height right)
        resOpacity = opacity left `comps` opacity right `comps`
            (if lM && rM then Opaque else Translucent)
        resPlace sWidth sHeight = (width, height, paint) where
            (lsWidth, rsWidth) = wDist sWidth
            (lsHeight, rsHeight) = hDist sHeight
            (lWidth, lHeight, lPaint') = place left lsWidth lsHeight
            (rWidth, rHeight, rPaint') = place right rsWidth rsHeight
            width = (+) <$> lWidth <*> rWidth
            height = max <$> lHeight <*> rHeight
            lpHeight = (-) <$> height <*> lHeight
            rpHeight = (-) <$> height <*> rHeight
            lPaint = if lM then lPaint'
                else paintVPad lpHeight lWidth lHeight lPaint'
            rPaint = if rM then rPaint'
                else paintVPad rpHeight rWidth rHeight rPaint'
            paint context = mix (lPaint context) $
                paintTrans ((\x -> (x, 0)) <$> lWidth) rPaint context
        res = Block {
            width = resWidth,
            height = resHeight,
            opacity = resOpacity,
            place = resPlace }
    (===) top bottom = res where
        (resWidth, tM, bM, wDist) = mediate (width top) (width bottom)
        (resHeight, hDist) = stack (height top) (height bottom)
        resOpacity = opacity top `comps` opacity bottom `comps`
            (if tM && bM then Opaque else Translucent)
        resPlace sWidth sHeight = (width, height, paint) where
            (tsWidth, bsWidth) = wDist sWidth
            (tsHeight, bsHeight) = hDist sHeight
            (tWidth, tHeight, tPaint') = place top tsWidth tsHeight
            (bWidth, bHeight, bPaint') = place bottom bsWidth bsHeight
            width = max <$> tWidth <*> bWidth
            height = (+) <$> tHeight <*> bHeight
            tpWidth = (-) <$> width <*> tWidth
            bpWidth = (-) <$> width <*> bWidth
            tPaint = if tM then tPaint'
                else paintHPad tpWidth tWidth tHeight tPaint'
            bPaint = if bM then bPaint'
                else paintHPad bpWidth bWidth bHeight bPaint'
            paint context = mix (tPaint context) $
                paintTrans ((\y -> (0, y)) <$> tHeight) bPaint context
        res = Block {
            width = resWidth,
            height = resHeight,
            opacity = resOpacity,
            place = resPlace }
    over high' low = res where
        res = case opacity high' of
            Opaque -> inner high' low
            Translucent -> high'
            Transparent ->
                let high = Markup.setBack (fst defaultAppearance) high'
                in inner high low
        inner high low = res where
            (resWidth, hhM, lhM, wDist) = mediate (width high) (width low)
            (resHeight, hvM, lvM, hDist) = mediate (height high) (height low)
            resOpacity = opacity low `comps`
                (if lhM && lvM then Opaque else Translucent)
            resPlace sWidth sHeight = (width, height, paint) where
                (hsWidth, lsWidth) = wDist sWidth
                (hsHeight, lsHeight) = hDist sHeight
                (hWidth, hHeight, hPaint') = place high hsWidth hsHeight
                (lWidth, lHeight, lPaint') = place low lsWidth lsHeight
                width = max <$> hWidth <*> lWidth
                height = max <$> hHeight <*> lHeight
                hpWidth = (-) <$> width <*> hWidth
                lpWidth = (-) <$> width <*> lWidth
                hpHeight = (-) <$> height <*> hHeight
                lpHeight = (-) <$> height <*> lHeight
                hPaint =
                    (if hvM then id else paintVPad hpHeight width hHeight) $
                    (if hhM then id else paintHPad hpWidth hWidth hHeight)
                    hPaint'
                lPaint =
                    (if lvM then id else paintVPad lpHeight width lHeight) $
                    (if lhM then id else paintHPad lpWidth hWidth hHeight)
                    lPaint'
                paint context = over (hPaint context) (lPaint context)
            res = Block {
                width = resWidth,
                height = resHeight,
                opacity = resOpacity,
                place = resPlace }
    setWidth width' block = case width block of
        Vary minWidth _ ->
            let nWidth = max width' <$> minWidth
            in explait $ block {
                width = Fix nWidth,
                place = \_ h -> place block nWidth h }
        _ -> block
    setHeight height' block = case height block of
        Vary minHeight _ ->
            let nHeight = max height' <$> minHeight
            in explait $ block {
                height = Fix nHeight,
                place = \w _ -> place block w nHeight }
        _ -> block
    setBack nBack block = block {
        opacity = Opaque,
        place = \w h ->
            let (rW, rH, paint) = place block w h
                nPaint c =
                    let nC = (\(_, offset) -> (Just nBack, offset)) <$> c
                    in paint nC
            in (rW, rH, nPaint) }

instance (Applicative f) => Markup.FlowToBlock
    Terminal (Flow f) (Block f) where
        blockify alignment' flow =
            let alignment = Flow.fromAlignment alignment'
            in Block {
                width = Vary (Flow.minWidth flow) 1,
                height = Dep,
                opacity = Transparent,
                place = \w _ ->
                    let (h, paint) = Flow.place flow alignment w
                    in (w, h, paint . ((\(Just b, p) -> (b, p)) <$>)) }
