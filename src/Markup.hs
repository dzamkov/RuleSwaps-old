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

-- Note: All figure decorators, such as 'setWidth', simply resolve ambiguity
-- in a figure and can not be used to overwrite known parameters. i.e.
-- only the first application of 'setWidth' may have an effect; after that,
-- there is no more ambiguity in the width.

-- | @a@ is a flow-like figure in the UI context @u@. A flow is a linear,
-- horizontal arrangement of items interspersed with potential breakpoints.
-- When applied to an area, the flow can be broken in to horizontal pieces
-- in order to fit. Flows are naturally translucent, having no defined
-- background. The monoid instances of @a@ allows concatenation of flows with
-- implied breakpoints between flows.
class (Context u, Monoid a) => Flow u a | a -> u where

    -- | Constructs a weak space of the given width, so called because it
    -- vanishes when it occurs adjacent to a break, regardless of how wide it
    -- is supposed to be.
    weakSpace :: Width u -> a

    -- | Constructs a strong space of the given width. Unlike a 'weakSpace', it
    -- will appear with its full width regardless of where it occurs.
    strongSpace :: Width u -> a

    -- | Constructs a figure displaying the given text, with no internal
    -- breakpoints.
    tightText :: Font u -> Material u -> String -> a

-- | Alias for 'weakSpace'.
space :: (Flow u a) => Width u -> a
space = weakSpace

-- | Constructs a figure displaying the given text with natural breakpoints
-- between each word.
text :: (Flow u a) => Font u -> Material u -> String -> a
text font fore = breakSpace where
    breakWord a [] = tightText font fore (reverse a)
    breakWord a (' ' : xs) = tightText font fore (reverse a) <> breakSpace xs
    breakWord a (x : xs) = breakWord (x : a) xs
    breakSpace [] = mempty
    breakSpace (' ' : xs) = breakSpace xs
    breakSpace (x : xs) = space (spaceWidth font) <> breakWord [x] xs

-- | @a@ is a block-like figure in the UI context @u@. A block is a
-- rectangular arrangement of items whose size may take a range of values,
-- and which may be translucent or opaque.
class Block u a | a -> u where

    -- | A completely transparent block with ambiguity in width and height.
    clear :: a

    -- | Places one block beside another, causing the heights to coincide.
    -- The method of distributing widths is undefined.
    (|||) :: a -> a -> a

    -- | Stacks one block on top of another, causing the widths to coincide.
    -- The method of distributing heights is undefined.
    (===) :: a -> a -> a

    -- | Places one block over another, causing the widths, heights, and
    -- positions to coincide. This has no effect if the first block is
    -- completely opaque and can fit entirely over the second.
    over :: a -> a -> a

    -- | Resolves the width of a block to be as close to the given value as
    -- possible.
    setWidth :: Width u -> a -> a

    -- | Resolves the height of a block to be as close to the given value as
    -- possible.
    setHeight :: Height u -> a -> a

    -- | Sets the background material for the transparent portions of a block.
    setBack :: Material u -> a -> a
