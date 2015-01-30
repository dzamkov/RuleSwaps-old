{-# LANGUAGE TypeFamilies #-}
module Terminal.Paint (
    Paintable (..),
    defer
) where

import Terminal.Draw
import Control.Monad.Identity
import Control.Applicative
import Data.Monoid

-- | @f@ is an applicative, with signal-like behavior, that allows the use
-- of @Paint f@ as an alternative to @f Draw@.
class Applicative f => Paintable f where

    -- | Similar to @f Draw@, but may allow partial updates, as is common with
    -- interactive interfaces.
    data Paint f

    -- | Converts a draw signal to a paint.
    toPaint :: f Draw -> Paint f

    -- | Converts a paint to a draw signal.
    fromPaint :: Paint f -> f Draw

    -- | Combines two paints assuming no overlap.
    mix :: Paint f -> Paint f -> Paint f
    mix = flip over

    -- | Combines two paints. Where overlap occurs, the first will take
    -- precedence.
    over :: Paint f -> Paint f -> Paint f
    over x y = toPaint $ (<>) <$> fromPaint y <*> fromPaint x

-- | 'Paint' analogue of 'join', applicable when @m@ is a 'Monad'.
defer :: (Monad m, Paintable m) => m (Paint m) -> Paint m
defer = toPaint . (>>= fromPaint)

instance Paintable Identity where
    data Paint Identity = IdentityPaint Draw
    toPaint = IdentityPaint . runIdentity
    fromPaint (IdentityPaint draw) = Identity draw
