{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Markup.Attr where

{-# ANN module "HLint: ignore Use String" #-}

-- | @p@ is a styling description that allows a color of type @c@ to be
-- specified.
class AttrColor c p | p -> c where

    -- | Applies the given color to a styling description.
    color :: c -> p -> p

-- | @p@ is a styling description for text that allows a font of type @f@
-- to be specified.
class AttrFont f p | p -> f where

    -- | Applies the given font to a styling description.
    font :: f -> p -> p

-- | @p@ is a styling description for an element that allows a title to be
-- specified.
class AttrTitle p where

    -- | Applies the given title to a styling description.
    title :: String -> p -> p

-- | @p@ is a styling description for an element which can be associated
-- a shortcut key.
class AttrKey p where
    {-# MINIMAL keys | key #-}

    -- | Applies one of the given keys (in order of preference) to a styling
    -- description.
    keys :: [Char] -> p -> p
    keys = key . head

    -- | Applies the given key to a styling description.
    key :: Char -> p -> p
    key = keys . (: [])

-- | @p@ is a styling description with a default value.
class HasDefault p where

    -- | The default style for @p@.
    defaultStyle :: p
