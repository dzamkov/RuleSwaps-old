{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
module Delta (
    Delta (..),
    keep,
    set,
    stride,
    DeltaRel (..),
    Complex,
    checkD,
    funD,
    fstD,
    sndD,
    plex2D,
    break2D
) where

import Control.Applicative

-- | A value of type @a@ associated with information about "how it got there".
-- A @Delta a@ contains the information needed to efficiently evolve a known
-- value of type @a@ to a new value.
data Delta a where

    -- | Indicates that the initial and final values are the same, and provides
    -- those values.
    Keep :: a -> Delta a

    -- | Specifies the final value only, giving no information about the
    -- progression of the value.
    Set :: a -> Delta a

    -- | Specifies the initial and final values, but gives no information about
    -- their relationship.
    Stride :: a -> a -> Delta a

    -- | Specifies an interesting type-dependent relationship between the
    -- initial and final values.
    Complex :: DeltaRel (Complex a) a => Complex a -> Delta a

deriving instance (Eq a, Eq (Complex a)) => Eq (Delta a)
deriving instance (Ord a, Ord (Complex a)) => Ord (Delta a)
deriving instance (Show a, Show (Complex a)) => Show (Delta a)
instance Functor Delta where
    fmap f (Keep x) = Keep (f x)
    fmap f (Set x) = Set (f x)
    fmap f (Stride x y) = Stride (f x) (f y)
    fmap f (Complex c) = case initial c of
        Just initial -> Stride (f initial) (f $ final c)
        Nothing -> Set (f $ final c)
instance Applicative Delta where
    pure = Keep
    (<*>) (Keep f) d = fmap f d
    (<*>) (Set f) d = Set (f $ final d)
    (<*>) (Stride f g) d = case initial d of
        Just initial -> Stride (f initial) (g $ final d)
        Nothing -> Set (g $ final d)
    (<*>) (Complex df) (Keep x) = df x
    (<*>) (Complex df) (Set x) = Set (final $ df x)
    (<*>) (Complex df) d =
        let fin = final $ df $ final d
        in case initial d of
            Just di -> case initial (df di) of
                Just initial -> Stride initial fin
                Nothing -> Set fin
            Nothing -> Set fin

-- | @d@ is a delta type for values of type @a@.
class DeltaRel d a | d -> a where

    -- | Gets the initial value of a delta, if known.
    initial :: d -> Maybe a
    initial _ = Nothing

    -- | Gets the final value of a delta.
    final :: d -> a

instance DeltaRel (Delta a) a where
    initial (Keep x) = Just x
    initial (Set _) = Nothing
    initial (Stride x _) = Just x
    initial (Complex c) = initial c
    final (Keep x) = x
    final (Set x) = x
    final (Stride _ x) = x
    final (Complex c) = final c

-- | Constructs a delta for a value that doesn't change.
keep :: a -> Delta a
keep = Keep

-- | Constructs a delta with a known final value but no information about
-- progression.
set :: a -> Delta a
set = Set

-- | Constructs a delta with a known initial and final value but no information
-- about progression.
stride :: a -> a -> Delta a
stride = Stride

-- | Describes the progression of values of type @a@ in an interesting way.
type family Complex a

-- Define complex delta's for common types.
type instance Complex (a -> b) = a -> Delta b
type instance Complex (a, b) = (Delta a, Delta b)
type instance Complex (a, b, c) = (Delta a, Delta b, Delta c)
instance DeltaRel (a -> Delta b) (a -> b) where
    initial _ = Nothing
    final f = final . f
instance DeltaRel (Delta a, Delta b) (a, b) where
    initial (x, y) = (,) <$> initial x <*> initial y
    final (x, y) = (final x, final y)
instance DeltaRel (Delta a, Delta b, Delta c) (a, b, c) where
    initial (x, y, z) = (,,) <$> initial x <*> initial y <*> initial z
    final (x, y, z) = (final x, final y, final z)

-- | Uses an 'Eq' instance to check whether the initial and final values of
-- a delta are the same, converting it to an instance of 'keep' if so.
checkD :: (Eq a) => Delta a -> Delta a
checkD d = case initial d of
    Just initial | initial == final d -> Keep initial
    _ -> d

-- | Converts a function to a delta into a delta of a function.
funD :: (a -> Delta b) -> Delta (a -> b)
funD = Complex

-- | Extracts the first element from a delta of a tuple.
fstD :: Delta (a, b) -> Delta a
fstD (Complex (dx, _)) = dx
fstD d = fst <$> d

-- | Extracts the second element from a delta of a tuple.
sndD :: Delta (a, b) -> Delta b
sndD (Complex (_, dy)) = dy
sndD d = snd <$> d

-- | Constructs a delta for a tuple.
plex2D :: Delta a -> Delta b -> Delta (a, b)
plex2D (Keep x) (Keep y) = Keep (x, y)
plex2D dx dy = Complex (dx, dy)

-- | Extracts the elements from a delta of a tuple.
break2D :: Delta (a, b) -> (Delta a, Delta b)
break2D (Complex (dx, dy)) = (dx, dy)
break2D d = (fst <$> d, snd <$> d)
