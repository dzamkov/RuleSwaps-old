{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Delta (
    Delta,
    IsDelta (..),
    DeltaRel (..),
    HasDelta,
    SimpDelta (..),
    PureDelta (..),
    toDelta
) where

import GHC.Exts (Constraint)
import Control.Applicative

-- | Describes a change in the value of an @a@.
type family Delta a

-- | Type @a@ is the delta type for some type.
class IsDelta a where

    -- | A delta that indicates no change.
    keep :: a

    -- | Returns true if the given delta is definitely 'keep'.
    isKeep :: a -> Bool
    isKeep _ = False

    -- | Combines two delta's.
    merge :: a -> a -> a

-- | @DeltaImpl a d@ is the constraint implied by @DeltaRel a d@
type family DeltaImpl a d :: Constraint

-- | Type @d@ is the delta type for type @a@.
class (DeltaImpl a d, IsDelta d) => DeltaRel a d | d -> a where

    -- | Applies a delta to a value to get the new value. If the delta was
    -- constructed using 'set', then this will not evaluate the initial value.
    apply :: d -> a -> a

    -- | Gets the simplest delta will, when applied to any value, yield the
    -- given value.
    set :: a -> d

-- | Type @a@ has a delta type.
type HasDelta a = DeltaRel a (Delta a)

-- | 'Deltor' is a specialization of 'Applicative' which associates each
-- element with a delta.
class Applicative f => Deltor f where

    -- | Constructs a deltor from a function which produces an "original" value
    -- and delta when given a function to get the "original" value and delta
    -- of another deltor.
    deltor :: (HasDelta b)
        => (forall g. (Applicative g) =>
            (forall a. (HasDelta a) => f a -> g (a, Delta a))
            -> g (b, Delta b))
        -> f b
    deltor = appDeltor

-- | An implementation of 'deltor' which can be applied to every applicative
-- functor. It assumes that the delta of every element is 'keep'.
appDeltor :: (Applicative f, HasDelta b)
    => (forall g. (Applicative g) =>
        (forall a. (HasDelta a) => f a -> g (a, Delta a))
        -> g (b, Delta b))
    -> f b
appDeltor inner = fst <$> inner ((\x -> (x, keep)) <$>)

-- | A simple delta type for type @a@ which just records whether the old value
-- is kept, or what value it is changed to, if it is changed.
data SimpDelta a = Keep | Set a deriving (Eq, Ord, Show)
instance IsDelta (SimpDelta a) where
    keep = Keep
    isKeep Keep = True
    isKeep _ = False
    merge x Keep = x
    merge _ x = x
type instance DeltaImpl a (SimpDelta a) = ()
instance DeltaRel a (SimpDelta a) where
    apply Keep x = x
    apply (Set x) _ = x
    set = Set

-- | A delta type for type @a@ which can either be a value of 'SimpDelta', or
-- of @Delta a@. This can be used when it is unknown whether @a@ satisfies the
-- constraint 'HasDelta'.
data PureDelta a where
    Simple :: SimpDelta a -> PureDelta a
    Complex :: (HasDelta a) => Delta a -> PureDelta a
instance IsDelta (PureDelta a) where
    keep = Simple Keep
    isKeep (Simple x) = isKeep x
    isKeep (Complex x) = isKeep x
    merge x (Simple Keep) = x
    merge _ x@(Simple (Set _)) = x
    merge (Simple Keep) x@(Complex _) = x
    merge (Simple (Set x)) (Complex dx) = Simple $ Set $ apply dx x
    merge (Complex x) (Complex y) = Complex $ merge x y
type instance DeltaImpl a (PureDelta a) = ()
instance DeltaRel a (PureDelta a) where
    apply (Simple x) y = apply x y
    apply (Complex x) y = apply x y
    set = Simple . Set

-- | Converts a value of type 'PureDelta' into the appropriate 'Delta'.
toDelta :: (HasDelta a) => PureDelta a -> Delta a
toDelta (Simple Keep) = keep
toDelta (Simple (Set x)) = set x
toDelta (Complex x) = x

-- Define Delta for functions.
instance IsDelta b => IsDelta (a -> b) where
    keep = const keep
    isKeep _ = False
    merge a b x = merge (a x) (b x)
type instance DeltaImpl (a -> b) (a -> c) = DeltaRel b c
instance DeltaRel b c => DeltaRel (a -> b) (a -> c) where
    apply f g x = apply (f x) (g x)
    set f x = set (f x)

-- Define Delta's for tuples.
instance (IsDelta a, IsDelta b) => IsDelta (a, b) where
    keep = (keep, keep)
    isKeep (x, y) = isKeep x && isKeep y
    merge (a, b) (c, d) = (merge a c, merge b d)
type instance DeltaImpl (a, b) (c, d) = (DeltaRel a c, DeltaRel b d)
instance (DeltaRel a c, DeltaRel b d) => DeltaRel (a, b) (c, d) where
    apply (a, b) (c, d) = (apply a c, apply b d)
    set (x, y) = (set x, set y)
instance (IsDelta a, IsDelta b, IsDelta c) => IsDelta (a, b, c) where
    keep = (keep, keep, keep)
    isKeep (x, y, z) = isKeep x && isKeep y && isKeep z
    merge (a, b, c) (d, e, f) = (merge a d, merge b e, merge c f)
type instance DeltaImpl (a, b, c) (d, e, f) =
    (DeltaRel a d, DeltaRel b e, DeltaRel c f)
instance (DeltaRel a d, DeltaRel b e, DeltaRel c f)
    => DeltaRel (a, b, c) (d, e, f) where
        apply (a, b, c) (d, e, f) = (apply a d, apply b e, apply c f)
        set (x, y, z) = (set x, set y, set z)

-- Define Delta's for basic types.
type instance Delta Bool = SimpDelta Bool
type instance Delta Int = SimpDelta Int
type instance Delta Integer = SimpDelta Integer
type instance Delta (a -> b) = a -> Delta b
type instance Delta (a, b) = (Delta a, Delta b)
type instance Delta (a, b, c) = (Delta a, Delta b, Delta c)
