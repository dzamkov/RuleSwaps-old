{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Terminal.Figure.Core (
    Layout,
    Placement,
    Bonus,
    Figure (..),
    figureS,
    placeS,
    layoutS,
    FigureLike (..),
    generalize,
    HasEmpty (..),
    CanTest (..),
    testWidth,
    testHeight
) where

import Stride
import Terminal.Draw
import Control.Monad.Identity
import Control.Applicative

-- | Contains known layout information for a figure with layout type @a@. This
-- information should include everything needed to decide where to place the
-- figure and surronding figures.
type family Layout a

-- | Contains placement information needed to draw a figure.
type family Placement a

-- | Contains extra information obtained while placing a figure.
type family Bonus a

-- | A figure that can be drawn to the terminal after sufficient placement
-- information is given. The layout type @a@ dictates what information is
-- provided and needs to be provided to the figure.
data Figure a = Figure {

    -- | The layout information provided by the figure.
    layout :: Layout a,

    -- | Provides placement information to a figure.
    place :: Placement a -> (Draw, Bonus a) }

data CFigure a = CFigure (Stride (Layout a))
    (Stride (Placement a -> (Draw, Bonus a)))
type instance Complex (Figure a) = CFigure a
instance IsStride (CFigure a) where
    glue (CFigure a b) (CFigure c d) = CFigure (glue a c) (glue b d)
instance StrideRel (CFigure a) (Figure a) where
    start (CFigure l p) = Figure {
        layout = start l,
        place = start p }
    end (CFigure l p) = Figure {
        layout = end l,
        place = end p }

-- | Constructs a stride for a figure.
figureS :: Stride (Layout a)
    -> Stride (Placement a -> (Draw, Bonus a))
    -> Stride (Figure a)
figureS layout place = Complex $ CFigure layout place

-- | Extracts the layout from a stride of a figure.
layoutS :: Stride (Figure a) -> Stride (Layout a)
layoutS (Complex (CFigure sl _)) = sl
layoutS s = layout <$> s

-- | Extracts the 'place' function from a stride of a figure.
placeS :: Stride (Figure a) -> Stride (Placement a -> (Draw, Bonus a))
placeS (Complex (CFigure _ sn)) = sn
placeS s = place <$> s

-- | @f@ is a type, of kind @* -> *@, which behaves like a figure, allowing
-- composition and decoration through the usual methods for figures.
class FigureLike f where

    -- | Produces a figure-like from a composition of existing figure-likes.
    compose :: (forall g. (Applicative g)
        => (forall a. f a -> g (Stride (Figure a)))
        -> g (Stride (Figure b))) -> f b

instance FigureLike Figure where
    compose inner = start $ runIdentity $ inner (Identity . stay)

-- | Converts a figure into a figure-like.
generalize :: (FigureLike f) => Figure a -> f a
generalize fig = compose (\_ -> pure $ stay fig)

-- | The layout type @a@ allows for empty figures which take up no space and
-- involves no drawing.
class HasEmpty a where

    -- | A figure which takes up no space and involves no drawing.
    empty :: Figure a

-- | Figures of type @a@ can be drawn directly for testing and debugging
-- purposes.
class CanTest a where
    test :: Figure a -> IO ()

-- | The width of variable-width figures drawn using 'test'.
testWidth :: Width
testWidth = 60

-- | The height of variable-height figures drawn using 'test'.
testHeight :: Height
testHeight = 30
