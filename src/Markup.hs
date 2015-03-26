{-# LANGUAGE FunctionalDependencies #-}
module Markup where

import Reactive
import Data.Monoid

-- | @a@ is a flow-like figure, a linear arrangment of items interspersed with
-- potential breakpoints. When applied to an area, the flow can be broken into
-- horizontal pieces in order to fit. The monoid instance of @a@ can be used
-- to concatenate flows with implicit breakpoints between them.
class Monoid a => Flow a where

    -- | Removes the potential breakpoints from a flow, ensuring that it will
    -- appear as an unbroken horizontal unit.
    tight :: a -> a

-- | @a@ is a flow-like figure to which horizontal space of a set absolute
-- width can be added. All spaces are by default "breaking" because
-- concatenation of flows created implicit breakpoints. 'tight' may be used
-- to create non-breaking spaces.
class (Monoid w, Flow a) => FlowSpace w a | a -> w where

    -- | Constructs a weak space of the given width, so called because it
    -- vanishes when it occurs adjacent to a break, regardless of how wide it
    -- is supposed to be.
    weakSpace :: w -> a

    -- | Constructs a strong space of the given width. Unlike a 'weakSpace', it
    -- will appear with its full width regardless of where it occurs.
    strongSpace :: w -> a

-- | Alias for 'weakSpace'.
space :: (FlowSpace w a) => w -> a
space = weakSpace

-- | @a@ is a flow-like figure with a means of displaying text. The text
-- can be styled using a description of type @p@.
class Flow a => FlowText p a | a -> p where

    -- | Constructs a figure displaying the given text with no internal
    -- breakpoints.
    tightText :: (p -> p) -> String -> a

    -- | A flow item corresponding to a space between words in text with
    -- the given styling description.
    naturalSpace :: (p -> p) -> a

-- | Constructs a figure displaying the given text with natural breakpoints
-- between each word.
text :: (FlowText p a) => (p -> p) -> String -> a
text style = breakSpace where
    breakWord a [] = tightText style (reverse a)
    breakWord a (' ' : xs) = tightText style (reverse a) <> breakSpace xs
    breakWord a (x : xs) = breakWord (x : a) xs
    breakSpace [] = mempty
    breakSpace (' ' : xs) = breakSpace xs
    breakSpace (x : xs) = naturalSpace style <> breakWord [x] xs

-- | @a@ is a block-like figure, appearing as a rectangle whose size may take a
-- range of values.
class Block a where

    -- | Places one block beside another, causing the heights to coincide.
    -- The method of distributing widths is undefined.
    infixl 3 |||
    (|||) :: a -> a -> a

    -- | Stacks one block on top of another, causing the widths to coincide.
    -- The method of distributing heights is undefined.
    infixl 2 ===
    (===) :: a -> a -> a

    -- | Removes as much space from as a block as possible without hiding
    -- any content.
    compact :: a -> a

-- | @a@ is a block-like figure which may be given an absolute size.
class Block a => BlockSize w h a | a -> w h where

    -- | Resolves the width of a block to be as close to the given value as
    -- possible without hiding any content.
    setWidth :: w -> a -> a

    -- | Resolves the height of a block to be as close to the given value as
    -- possible without hiding any content.
    setHeight :: h -> a -> a

-- | @a@ is a block-like figure that allows the construction of solid-color
-- blocks.
class Block a => BlockSolid c a | a -> c where

    -- | Constructs a solid-color block of the given color.
    solid :: c -> a

-- | @a@ is a block-like figure that may be partially transparent.
class Block a => BlockTrans a where

    -- | A completely transparent block with ambiguity in width and height.
    clear :: a

    -- | Places one block over another, causing the widths, heights, and
    -- positions to coincide. This has no effect if the first block is
    -- completely opaque and can fit entirely over the second.
    over :: a -> a -> a

-- | Surronds a block with a transparent border of variable size, allowing
-- the inner and outer sizes to be vary independently.
inset :: (BlockTrans a) => a -> a
inset inner = clear === clear ||| inner ||| clear === clear

-- | Applies padding to a block, given the size of the padding in the left,
-- top, right and bottom directions.
pad :: (BlockSize w h a, BlockTrans a) => w -> h -> w -> h -> a -> a
pad l' t' r' b' inner = res where
    l = setWidth l' clear
    t = setHeight t' clear
    r = setWidth r' clear
    b = setHeight b' clear
    res = t === l ||| inner ||| r === b

-- | Sets the color of the transparent portions of a block.
setBack :: (BlockSolid c a, BlockTrans a) => c -> a -> a
setBack color hi = over hi $ solid color

-- | @a@ is a block-like figure to which a border can be applied.
class Block a => BlockBorder p a | a -> p where

    -- | Applies a border to a block.
    withBorder :: (p -> p) -> a -> a

-- | Identifies a possible alignment for the lines within a flow.
data Alignment
    = Left
    | Center
    | Right
    | Justify

-- | @a@ is a 'Flow' figure that can be converted into a 'Block' figure of
-- type @b@.
class (Flow a, Block b) => FlowToBlock a b | a -> b, b -> a where

    -- | Converts a flow into a translucent block using the given alignment.
    blockify :: Alignment -> a -> b

-- | @w a@ is a description of a interactive figure whose running instances
-- produce a value of type @a@.
class Functor w => Widget w where

    -- | Computes the fixed point of a widget.
    wfix :: (a -> w a) -> w a

-- | @w@ is a widget type that allows the construction of buttons.
class (Event e, Widget w) => WidgetButton e p w | w -> e p where

    -- | Constructs a button widget with the given style. The resulting
    -- event will occur whenever the button is pressed.
    button :: (p -> p) -> w (e ())
