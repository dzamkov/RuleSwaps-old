{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Terminal.Flow (
    TextStyle,
    Flow,
    minWidth,
    place,
    Alignment,
    left,
    center,
    right,
    fromAlignment
) where

import Prelude hiding (break)
import qualified Markup
import Markup.Attr
import Terminal.Base
import Terminal.Paint
import qualified Terminal.Draw as Draw
import Data.Monoid
import Data.Traversable
import Data.Maybe (fromMaybe)
import Control.Monad.State
import Control.Applicative

-- | Describes a space in a flow by giving its width.
type Space = Width

-- | Describes a word in a flow.
type Word f = f (Color, Point) -> Paint f

-- | A possible style for text in a flow.
data TextStyle = TextStyle { textColor :: Color }
instance AttrColor Color TextStyle where
    color c style = style { textColor = c }
instance HasDefault TextStyle where
    defaultStyle = TextStyle { textColor = snd defaultAppearance }

-- | A figure, based in the terminal, which is linear and can be broken up at
-- certain points, much like text.
data Flow f = Flow Space [(Word f, f Width, Space)]
instance Monoid (Flow f) where
    mempty = Flow 0 []
    mappend (Flow px []) (Flow py yi) = Flow (px + py) yi
    mappend (Flow px (xi : xis)) y = Flow px (go xi xis y) where
        go (d, w, s) [] (Flow py yi) = (d, w, s + py) : yi
        go xi (nXi : xis) y = xi : go nXi xis y
instance Applicative f => Markup.Flow Width (Flow f) where
    weakSpace width = Flow width []
    strongSpace width = res where
        paint (back, offset) = Draw.space back offset width
        res = Flow 0 [(toPaint . (paint <$>), pure width, 0)]
    tight = error "'tight' not implemented" -- TODO
instance Applicative f => Markup.FlowText Width TextStyle (Flow f) where
    tightText style str = res where
        fore = textColor (style defaultStyle)
        width = Width $ length str
        paint (back, offset) = Draw.string (back, fore) offset str
        res = Flow 0 [(toPaint . (paint <$>), pure width, 0)]
    naturalSpace _ = Markup.space 1

-- | Describes a possible alignment of flow items within a line.
newtype Alignment = Alignment {

    -- | Determines the offsets of all items within a line using this
    -- alignment.
    align :: forall t. (Traversable t)
        => Width -- ^ target width of the line
        -> t (Width, Space) -- ^ item and space widths
        -> Width -- ^ total item and space width
        -> t X -- ^ final item offsets in the line

    }

-- | Constructs an alignment which places items as close together as possible.
-- The given function will determine the offset of the first item in the line
-- given the maximum offset.
compact :: (X -> X) -> Alignment
compact offset = Alignment $ \target line width ->
    let x = offset (target - width)
    in flip evalState x $
        traverse (\(width, space) -> do
            x <- get
            let nX = x + width + space
            put nX
            return x) line

-- | Aligns all items in a line to the leftmost possible offset.
left :: Alignment
left = compact (const 0)

-- | Aligns all items in a line to the rightmost possible offset.
right :: Alignment
right = compact id

-- | Aligns all items compactly in the center.
center :: Alignment
center = compact (Width . (`div` 2) . cells)

-- | Converts a 'Markup.Alignment' into an alignment a flow can use.
fromAlignment :: Markup.Alignment -> Alignment
fromAlignment Markup.Left = left
fromAlignment Markup.Center = center
fromAlignment Markup.Right = right
fromAlignment Markup.Justify = center -- TODO

-- | An applicative interface for flow layout, defined by 'runLayout', 'spaceL'
-- and 'wordL'.
data Layout f a = Layout {

    -- | Performs layout, given the alignment, target line width,
    -- initial forward state, and initial backward state.
    runLayout :: Alignment -> f Width
        -> f (Width, Y, [(Width, Space)]) -> f [X]
        -> (a, f (Width, Y, [(Width, Space)]), f [X]) }

instance Functor (Layout f) where
    fmap f layout = Layout $ \alignment width fw bw ->
        let (res, nFw, nBw) = runLayout layout alignment width fw bw
        in (f res, nFw, nBw)
instance Applicative (Layout f) where
    pure res = Layout $ \_ _ fw bw -> (res, fw, bw)
    (<*>) a b = Layout $ \alignment width fw bw ->
        let (aRes, iFw, nBw) = runLayout a alignment width fw iBw
            (bRes, nFw, iBw) = runLayout b alignment width iFw bw
        in (aRes bRes, nFw, nBw)

-- | Utility function for 'Layout's. Given the forward state for a full line,
-- gets the backward state for the line.
finishLine :: Alignment -> Width
    -> (Width, Y, [(Width, Space)]) -> [X]
finishLine _ _ (_, _, []) = []
finishLine alignment target (x, _, (w, s) : items) = reverse $
    align alignment target (reverse ((w, 0) : items)) (x - s)

-- | Performs layout using default forward and backward state. Returns
-- the results and the final height.
runFixLayout :: (Applicative f)
    => Layout f a
    -> Alignment -> f Width
    -> (a, f Height)
runFixLayout layout alignment target = (res, height) where
    fw = pure (Width 0, Height 0, [])
    bw = finishLine alignment <$> target <*> nFw
    (res, nFw, _) = runLayout layout alignment target fw bw
    height = (\(_, y, items) -> if null items then y else y + 1) <$> nFw

-- | Designates a breaking space in a layout.
spaceL :: (Applicative f) => f Space -> Layout f ()
spaceL space = Layout $ \alignment target fw bw ->
    let nS = (\target space (x, y, items) -> case (x, items) of
          (_, []) -> ((0, y, []), Just [])
          (x, items) | x + space >= target ->
            ((0, y + 1, []), Just $ finishLine alignment target (x, y, items))
          (x, (w, s) : items) ->
            ((x + space, y, (w, s + space) : items), Nothing))
          <$> target <*> space <*> fw
    in ((), fst <$> nS, fromMaybe <$> bw <*> (snd <$> nS))

-- | Designates a word (non-breaking) in a layout. The offset of the word in
-- the layout is returned.
wordL :: (Applicative f) => f Width -> Layout f (f Offset)
wordL width = Layout $ \alignment target fw bw ->
    let nS = (\target width (x, y, items) -> case x of
            x | x + width >= target -> ((width, y + 1, [(width, 0)]),
                Just $ finishLine alignment target (x, y, items))
            x -> ((x + width, y, (width, 0) : items), Nothing))
            <$> target <*> width <*> fw
        nFw = fst <$> nS
    in ((\(x : _) (_, y, _) -> (x, y)) <$> bw <*> nFw,
        nFw, fromMaybe <$> (tail <$> bw) <*> (snd <$> nS))

-- | Gets the minimum width that the given flow can be placed with.
minWidth :: (Applicative f) => Flow f -> f Width
minWidth (Flow _ xs) = foldr (\(_, w, _) a -> max <$> a <*> w) (pure 0) xs

-- | Places a flow, giving its alignment and width (which must be at least
-- 'minWidth'). Returns the final height of the flow, along with a function
-- which draws it given back color and absolute offset.
place :: (Applicative f) => Flow f
    -> Alignment -> f Width
    -> (f Height, f (Color, Point) -> Paint f)
place (Flow _ items) alignment target = res where
    clear pOffset nOffset = toPaint .
        ((\(px, py) (nx, ny) target (back, (x, y)) ->
            if py == ny then Draw.space back (x + px, y + py) (nx - px)
            else Draw.space back (x + px, y + py) (target - px) <>
                Draw.space back (x, y + ny) nx)
        <$> pOffset <*> nOffset <*> target <*>)
    addWidth (x, y) width = (x + width, y)
    transform (tx, ty) (back, (x, y)) = (back, (tx + x, ty + y))
    placeItems [] = pure clear
    placeItems ((word, width, space) : items) =
        (\offset placeRem pOffset nOffset context ->
            clear pOffset offset context `mix`
            word (transform <$> offset <*> context) `mix`
            placeRem (addWidth <$> offset <*> width) nOffset context)
        <$> (wordL width <* spaceL (pure space)) <*> placeItems items
    (l, height) = runFixLayout (placeItems items) alignment target
    pOffset = pure (Width 0, Height 0)
    nOffset = (\h -> (0, h)) <$> height
    res = (height, l pOffset nOffset)
