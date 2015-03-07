{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Terminal.Metrics where

import qualified System.Console.ANSI as ANSI
import Data.Monoid

-- | Describes a horizontal length (number of cells) in the terminal.
newtype Width = Width Int
    deriving (Eq, Ord, Enum, Show, Num, Real, Integral, Bounded)
instance Monoid Width where
    mempty = 0
    mappend = (+)

-- | Describes a vertical length (number of cells) in the terminal.
newtype Height = Height Int
    deriving (Eq, Ord, Enum, Show, Num, Real, Integral, Bounded)
instance Monoid Height where
    mempty = 0
    mappend = (+)

-- | @a@ is a length in the context of a terminal.
class IsLength a where

    -- | Gets the number of cells in the given length.
    cells :: a -> Int

instance IsLength Width where
    cells (Width x) = x
instance IsLength Height where
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
data Color = Color ANSI.ColorIntensity ANSI.Color
    deriving (Eq, Ord, Show)

-- | Describes the appearance of a glyph on the terminal.
type Appearance = (Color, Color)

-- | The default appearance for some terminal.
defaultAppearance :: Appearance
defaultAppearance = (Color ANSI.Dull ANSI.Black, Color ANSI.Dull ANSI.White)
