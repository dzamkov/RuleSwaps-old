{-# LANGUAGE FunctionalDependencies #-}
module Markup where

import Data.Monoid

-- | @a@ is a flow-like figure, a linear arrangment of items interspersed with
-- potential breakpoints. When applied to an area, the flow can be broken into
-- horizontal pieces in order to fit. The monoid instance of @a@ can be used
-- to concatenate flows with implicit breakpoints between them.
class (Monoid w, Monoid a) => Flow w a | a -> w where

    -- | Constructs a weak space of the given width, so called because it
    -- vanishes when it occurs adjacent to a break, regardless of how wide it
    -- is supposed to be.
    weakSpace :: w -> a

    -- | Constructs a strong space of the given width. Unlike a 'weakSpace', it
    -- will appear with its full width regardless of where it occurs.
    strongSpace :: w -> a

    -- | Removes the potential breakpoints from a flow, ensuring that it will
    -- appear as an unbroken horizontal unit.
    tight :: a -> a

-- | Alias for 'weakSpace'.
space :: (Flow w a) => w -> a
space = weakSpace

-- | @p@ is a styling description that allows a color of type @c@ to be
-- specified.
class Monoid p => AttrColor c p | p -> c where

    -- | Constructs a styling description with the given color.
    color :: c -> p

-- | @p@ is a styling description for text that allows a font of type @f@
-- to be specified.
class Monoid p => AttrFont f p | p -> f where

    -- | Constructs a styling description with the given font.
    font :: f -> p

-- | @a@ is a flow-like figure with a means of displaying text. The text
-- can be styled using a description of type @p@.
class (Flow w a, Monoid p) => FlowText w p a | a -> p where

    -- | Constructs a figure displaying the given text with no internal
    -- breakpoints.
    tightText :: p -> String -> a

    -- | A flow item corresponding to a space between words in text with
    -- the given styling description.
    naturalSpace :: p -> a

-- | Constructs a figure displaying the given text with natural breakpoints
-- between each word.
text :: (FlowText w p a) => p -> String -> a
text style = breakSpace where
    breakWord a [] = tightText style (reverse a)
    breakWord a (' ' : xs) = tightText style (reverse a) <> breakSpace xs
    breakWord a (x : xs) = breakWord (x : a) xs
    breakSpace [] = mempty
    breakSpace (' ' : xs) = breakSpace xs
    breakSpace (x : xs) = naturalSpace style <> breakWord [x] xs

-- | @a@ is a block-like figure, appearing as a rectangle whose size may take a
-- range of values.
class Block w h a | a -> w h where

    -- | Places one block beside another, causing the heights to coincide.
    -- The method of distributing widths is undefined.
    (|||) :: a -> a -> a

    -- | Stacks one block on top of another, causing the widths to coincide.
    -- The method of distributing heights is undefined.
    (===) :: a -> a -> a

    -- | Resolves the width of a block to be as close to the given value as
    -- possible without hiding any content.
    setWidth :: w -> a -> a

    -- | Resolves the height of a block to be as close to the given value as
    -- possible without hiding any content.
    setHeight :: h -> a -> a

-- | @a@ is a block-like figure that allows the construction of solid-color
-- blocks.
class Block w h a => BlockSolid w h c a | a -> c where

    -- | Constructs a solid-color block of the given color.
    solid :: c -> a

-- | @a@ is a block-like figure that may be partially transparent.
class Block w h a => BlockTrans w h a where

    -- | A completely transparent block with ambiguity in width and height.
    clear :: a

    -- | Places one block over another, causing the widths, heights, and
    -- positions to coincide. This has no effect if the first block is
    -- completely opaque and can fit entirely over the second.
    over :: a -> a -> a

-- | Sets the color of the transparent portions of a block.
setBack :: (BlockSolid w h c a, BlockTrans w h a) => c -> a -> a
setBack color hi = over hi $ solid color

-- | Identifies a possible alignment for the lines within a flow.
data Alignment
    = Left
    | Center
    | Right
    | Justify

-- | @a@ is a 'Flow' figure that can be converted into a 'Block' figure of
-- type @b@.
class (Flow w a, Block w h b) => FlowToBlock w h a b | a -> b where

    -- | Converts a flow into a translucent block using the given alignment.
    blockify :: Alignment -> a -> b
