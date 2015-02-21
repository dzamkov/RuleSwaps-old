{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Terminal.Context (
    Terminal,
    Length (..),
    Material (..),
    Font (..),
    Width,
    Height,
    X, Y,
    Offset,
    Point,
    IsLength (..),
    Color,
    Appearance,
    defaultAppearance
) where

import Markup hiding (Width, Height)
import qualified Markup
import qualified System.Console.ANSI as ANSI

-- | Identifies the UI 'Context' for a terminal.
data Terminal

{-# ANN module "HLint: ignore Unused LANGUAGE pragma" #-}
instance Context Terminal where
    newtype Length Terminal Horizontal = Width Int
        deriving (Eq, Ord, Enum, Show, Num, Real, Integral, Bounded)
    newtype Length Terminal Vertical = Height Int
        deriving (Eq, Ord, Enum, Show, Num, Real, Integral, Bounded)
    data Material Terminal = Color ANSI.ColorIntensity ANSI.Color
        deriving (Eq, Ord, Show)
    data Font Terminal = Font
    spaceWidth _ = Width 1

-- | Describes a width in a terminal (number of horizontal cells).
type Width = Markup.Width Terminal

-- | Describes a height in a terminal (number of vertical cells).
type Height = Markup.Height Terminal

-- | @a@ is a length in the context of a terminal.
class IsLength a where

    -- | Gets the number of cells in the given length.
    cells :: a -> Int

instance IsLength (Markup.Length Terminal Horizontal) where
    cells (Width x) = x
instance IsLength (Markup.Length Terminal Vertical) where
    cells (Height x) = x

-- | Describes a horizontal offset in the terminal.
type X = Width

-- | Describes a vertical offset in the terminal.
type Y = Height

-- | Describes an offset in a terminal.
type Offset = (X, Y)

-- | Describes a point in a terminal.
type Point = Offset

-- | Describes a color in a terminal.
type Color = Markup.Material Terminal

-- | Describes the appearance of a glyph on the terminal.
type Appearance = (Color, Color)

-- | The default appearance for some terminal.
defaultAppearance :: Appearance
defaultAppearance = (Color ANSI.Dull ANSI.Black, Color ANSI.Dull ANSI.White)
