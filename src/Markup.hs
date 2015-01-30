{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
module Markup where

import Data.Monoid

-- | Identifies an axis.
data Axis
    = Horizontal
    | Vertical

-- | Type @u@ is a geometric and visual context for a UI that can be created
-- using markup commands.
class Context u where

    -- | Describes a length along a given axis in the UI system @u@.
    data Length u (a :: Axis) :: *

    -- | Describes a uniform visual material that can be applied to a figure
    -- taking a non-zero area.
    data Material u :: *

    -- | Identifies/describes a font that can be applied to text in this
    -- context.
    data Font u :: *

    -- | The width of a natural space in the given font.
    spaceWidth :: Font u -> Width u

-- | Identifies a length along the horizontal axis in the UI system @u@.
type Width u = Length u Horizontal

-- | Identifies a length along the vertical axis in the UI system @u@.
type Height u = Length u Vertical

-- | @f@ is a flow-like figure in the UI context @u@. A flow is a linear,
-- horizontal arrangement of items interspersed with potential breakpoints.
-- When applied to an area, the flow can be broken in to horizontal pieces
-- in order to fit. Flows are naturally translucent, having no defined
-- background. The monoid instances of @f@ allows concatenation of flows with
-- implied breakpoints between flows.
class (Context u, Monoid f) => Flow u f | f -> u where

    -- | Constructs a weak space of the given width, so called because it
    -- vanishes when it occurs adjacent to a break, regardless of how wide it
    -- is supposed to be.
    weakSpace :: Width u -> f

    -- | Constructs a strong space of the given width. Unlike a 'weakSpace', it
    -- will appear with its full width regardless of where it occurs.
    strongSpace :: Width u -> f

    -- | Constructs a figure displaying the given text, with no internal
    -- breakpoints.
    tightText :: Font u -> Material u -> String -> f

-- | Alias for 'weakSpace'.
space :: (Flow u f) => Width u -> f
space = weakSpace

-- | Constructs a figure displaying the given text with natural breakpoints
-- between each word.
text :: (Flow u f) => Font u -> Material u -> String -> f
text font fore = breakSpace where
    breakWord a [] = tightText font fore (reverse a)
    breakWord a (' ' : xs) = tightText font fore (reverse a) <> breakSpace xs
    breakWord a (x : xs) = breakWord (x : a) xs
    breakSpace [] = mempty
    breakSpace (' ' : xs) = breakSpace xs
    breakSpace (x : xs) = space (spaceWidth font) <> breakWord [x] xs
