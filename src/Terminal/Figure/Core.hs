{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
module Terminal.Figure.Core (
    Layout,
    Placement,
    Bonus,
    Figure (..),
    figureD,
    placeD,
    layoutD,
    Axis (..),
    ComposeHint,
    FigureLike (..),
    generalize,
    HasEmpty (..),
    CanTest (..),
    testWidth,
    testHeight
) where

import Delta
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

data DFigure a = DFigure (Delta (Layout a))
    (Delta (Placement a -> (Draw, Bonus a)))
type instance Complex (Figure a) = DFigure a
instance DeltaRel (DFigure a) (Figure a) where
    initial (DFigure l p) = (\l p -> Figure { layout = l, place = p })
        <$> initial l <*> initial p
    final (DFigure l p) = Figure {
        layout = final l,
        place = final p }

-- | Constructs a delta for a figure.
figureD :: Delta (Layout a)
    -> Delta (Placement a -> (Draw, Bonus a))
    -> Delta (Figure a)
figureD layout place = Complex $ DFigure layout place

-- | Extracts the layout from a delta of a figure.
layoutD :: Delta (Figure a) -> Delta (Layout a)
layoutD (Complex (DFigure sl _)) = sl
layoutD s = layout <$> s

-- | Extracts the 'place' function from a delta of a figure.
placeD :: Delta (Figure a) -> Delta (Placement a -> (Draw, Bonus a))
placeD (Complex (DFigure _ sn)) = sn
placeD s = place <$> s

-- | Identifies an axis.
data Axis
    = Horizontal
    | Vertical

-- | Context information that may be given while composing a figure.
type ComposeHint = Maybe Axis

-- | @f@ is a type, of kind @* -> *@, which behaves like a figure, allowing
-- composition and decoration through the usual methods for figures.
class FigureLike f where

    -- | Produces a figure-like from a composition of existing figure-likes.
    compose :: ComposeHint -> (forall g. (Applicative g)
        => (forall a. f a -> g (Delta (Figure a)))
        -> g (Delta (Figure b))) -> f b

instance FigureLike Figure where
    compose _ inner = final $ runIdentity $ inner (Identity . keep)

-- | Converts a figure into a figure-like.
generalize :: (FigureLike f) => Figure a -> f a
generalize fig = compose Nothing (\_ -> pure $ keep fig)

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
