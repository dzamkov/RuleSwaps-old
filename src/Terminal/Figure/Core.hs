{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Terminal.Figure.Core (
    Layout,
    Placement,
    Next,
    Figure (..),
    dfigure,
    dplace,
    dlayout,
    Static,
    static,
    draw,
    dstatic,
    ddraw,
    HasEmpty (..),
    CanTest (..),
    testWidth,
    testHeight
) where

import Stride
import Terminal.Draw
import Control.Applicative

-- | Contains known layout information for a figure with layout type @a@. This
-- is information provided by the figure to the client.
type family Layout a

-- | Contains placement information needed to "evolve" a figure from layout
-- type @a@ to the next layout type. This is information provided to the
-- figure by the client.
type family Placement a

-- | The layout type a figure with the given layout type "evolves" to when
-- provided placement information.
type family Next a

-- | A figure that can be drawn to the terminal after sufficient placement
-- information is given. The layout type @a@ dictates what information is
-- provided and needs to be provided to the figure.
data Figure a = Figure {

    -- | The layout information provided by the figure.
    layout :: Layout a,

    -- | Provides placement information to a figure, evolving it to another
    -- layout type.
    place :: Placement a -> Figure (Next a) }

data CFigure a = CFigure (Stride (Layout a))
    (Stride (Placement a -> Figure (Next a)))
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
dfigure :: Stride (Layout a)
    -> (Placement a -> Stride (Figure (Next a)))
    -> Stride (Figure a)
dfigure layout place = Complex $ CFigure layout (dfun place)

-- | Extracts the layout from a deltor of a figure.
dlayout :: (Deltor f) => f (Figure a) -> f (Layout a)
dlayout source = deltor (\eval -> extract <$> eval source) where
    extract (Complex (CFigure dl _)) = dl
    extract s = layout <$> s

-- | Places a figure within the context of a deltor.
dplace :: (Deltor f) => f (Figure a) -> f (Placement a) -> f (Figure (Next a))
dplace fig p = deltor (\eval -> extract <$> eval fig <*> eval p) where
    extract (Complex (CFigure _ dn)) p = dn <*> p
    extract f p = place <$> f <*> p

-- | The layout type for a figure which has is equivalent to 'Draw'. Static
-- figures can not be moved or resized and have positioning described in
-- absolute terminal coordinates.
data Static

type instance Layout Static = Draw
type instance Placement Static = ()
type instance Next Static = Static

-- | Constructs a static figure from the given draw operation.
static :: Draw -> Figure Static
static draw = fig where
    fig = Figure {
        layout = draw,
        place = const fig }

-- | Gets the draw operation for a static figure.
draw :: Figure Static -> Draw
draw = layout

-- | Constructs a static figure from a draw operation within the context of
-- a deltor.
dstatic :: (Deltor f) => f Draw -> f (Figure Static)
dstatic draw = deltor (\eval -> cons <$> eval draw) where
    cons draw =
        let fig = Complex $ CFigure draw (const <$> fig)
        in fig

-- | Gets the draw operation for a static figure within the context of a
-- deltor.
ddraw :: (Deltor f) => f (Figure Static) -> f Draw
ddraw = dlayout

-- | The layout type @a@ allows for empty figures which take up no space and
-- involves no drawing.
class HasEmpty a where

    -- | A figure which takes up no space and involves no drawing.
    empty :: Figure a

instance HasEmpty Static where
    empty = static none

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

instance CanTest Static where
    test fig = runDrawInline testHeight $ draw fig
