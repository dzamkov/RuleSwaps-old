{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Delta (
    Delta,
    IsDelta (..),
    DeltaRel (..),
    HasDelta,
    DeltaMap,
    deltaMap,
    eqDeltaMap,
    idDeltaMap,
    undeltaMap,
    SimpDelta (..)
) where

-- | Describes a change in the value of an @a@.
type family Delta a

-- | Type @a@ is the delta type for some type.
class IsDelta a where

    -- | A delta that indicates no change.
    keep :: a

    -- | Returns true if the given delta is definitely 'keep'.
    isKeep :: a -> Bool

    -- | Combines two delta's.
    merge :: a -> a -> a

-- | Type @d@ is the delta type for type @a@.
class IsDelta d => DeltaRel a d | d -> a where

    -- | Applies a delta to a value to get the new value. If the delta was
    -- constructed using 'set', then this will not evaluate the initial value.
    apply :: d -> a -> a

    -- | Gets the simplest delta will, when applied to any value, yield the
    -- given value.
    set :: a -> d

-- | Type @a@ has a delta type.
type HasDelta a = DeltaRel a (Delta a)

-- | A mapping function between @a@ and @b@ that can use delta's.
type DeltaMap a b = a -> Delta a -> Maybe b -> Delta b

-- | Converts a regular mapping function into a 'DeltaMap'.
deltaMap :: (HasDelta a, HasDelta b) => (a -> b) -> DeltaMap a b
deltaMap f oi di _ =
    let ni = apply di oi
        no = f ni
    in set no

-- | Converts a regular mapping function into a 'DeltaMap' that takes into
-- account equailty of output values in order to sometimes produce 'keep'.
eqDeltaMap :: (HasDelta a, HasDelta b, Eq b) => (a -> b) -> DeltaMap a b
eqDeltaMap f oi di oo =
    let ni = apply di oi
        no = f ni
    in case oo of
        Just oo | no == oo -> keep
        _ -> set no

-- | The identity delta map.
idDeltaMap :: DeltaMap a a
idDeltaMap _ di _ = di

-- | Converts a 'DeltaMap' into a regular mapping function.
undeltaMap :: (HasDelta a, HasDelta b) => DeltaMap a b -> a -> b
undeltaMap f i = apply (f undefined (set i) Nothing) undefined

-- | A simple delta type for type @a@ which just records whether the old value
-- is kept, or what value it is changed to, if it is changed.
data SimpDelta a = Keep | Set a deriving (Eq, Ord, Show)
instance IsDelta (SimpDelta a) where
    keep = Keep
    isKeep Keep = True
    isKeep _ = False
    merge Keep x = x
    merge x _ = x
instance DeltaRel a (SimpDelta a) where
    apply Keep x = x
    apply (Set x) _ = x
    set = Set

-- Define Delta's for tuples.
instance (IsDelta a, IsDelta b) => IsDelta (a, b) where
    keep = (keep, keep)
    isKeep (x, y) = isKeep x && isKeep y
    merge (a, b) (c, d) = (merge a c, merge b d)
instance (DeltaRel a c, DeltaRel b d) => DeltaRel (a, b) (c, d) where
    apply (a, b) (c, d) = (apply a c, apply b d)
    set (x, y) = (set x, set y)
instance (IsDelta a, IsDelta b, IsDelta c) => IsDelta (a, b, c) where
    keep = (keep, keep, keep)
    isKeep (x, y, z) = isKeep x && isKeep y && isKeep z
    merge (a, b, c) (d, e, f) = (merge a d, merge b e, merge c f)
instance (DeltaRel a d, DeltaRel b e, DeltaRel c f)
    => DeltaRel (a, b, c) (d, e, f) where
        apply (a, b, c) (d, e, f) = (apply a d, apply b e, apply c f)
        set (x, y, z) = (set x, set y, set z)

-- Define Delta's for basic types.
type instance Delta Bool = SimpDelta Bool
type instance Delta Int = SimpDelta Int
type instance Delta Integer = SimpDelta Integer
type instance Delta (a, b) = (Delta a, Delta b)
type instance Delta (a, b, c) = (Delta a, Delta b, Delta c)
