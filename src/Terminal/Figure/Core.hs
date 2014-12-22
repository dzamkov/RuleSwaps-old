{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Terminal.Figure.Core (
    Layout,
    Placement,
    Figure (..),
    Static,
    static,
    draw,
    To (..),
    HasEmpty (..),
    CanTest (..),
    testWidth,
    testHeight
) where

import Terminal.Draw

-- | Contains known layout information for a figure with layout type @a@. This
-- is information provided by the figure to the client.
type family Layout a

-- | Contains placement information needed to "evolve" a figure from layout
-- type @a@ to the given layout type. This is information provided to the
-- figure by the client.
type family Placement a :: * -> *

-- | A figure that can be drawn to the terminal after sufficient placement
-- information is given. The layout type @a@ dictates what information is
-- provided and needs to be provided to the figure.
data Figure a = Figure {

    -- | The layout information provided by the figure.
    layout :: Layout a,

    -- | Provides placement information to a figure, evolving it to another
    -- layout type.
    place :: forall b. Placement a b -> Figure b }

-- | The layout type for a figure which has is equivalent to 'Draw'. Static
-- figures can not be moved or resized and have positioning described in
-- absolute terminal coordinates.
data Static

data ConstVoid :: * -> *
type instance Layout Static = Draw
type instance Placement Static = ConstVoid

-- | Constructs a static figure from the given draw operation.
static :: Draw -> Figure Static
static draw = fig where
    fig = Figure {
        layout = draw,
        place = undefined }

-- | Gets the draw operation for a static figure.
draw :: Figure Static -> Draw
draw = layout

-- | A utility type that is potentially useful in defining placements. This
-- wraps a type @a@ of kind @*@ as the type @To a b@ which has kind @* -> *@
-- and is only inhabited when the next type argument is @b@. In practice, a
-- placement can be defined as follows:
-- > type instance Placement A = To APlacement B
-- in order to state that layout type @A@ can only be evolved to layout type
-- @B@, and that requires placement information of the concrete type
-- @APlacement@.
data To a b c where
    To :: a -> To a b b

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
