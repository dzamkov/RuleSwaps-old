{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Stride (
    Stride (..),
    Complex,
    IsStride (..),
    StrideRel (..),
    stride,
    stay,
    Delta,
    set,
    keep,
    apply,
    Evaluator,
    Deltor (..),
    dcheck
) where

import Control.Monad.Identity (Identity (..))
import Control.Applicative

-- | Describes the progression of a value of type @a@ between two points
-- ('start' and 'end').
data Stride a where

    -- | A stride between two points where nothing is known about their
    -- relationship.
    Stride :: a -> a -> Stride a

    -- | A stride between two points with identical values.
    Stay :: a -> Stride a

    -- | An interesting stride between two points.
    Complex :: StrideRel (Complex a) a => Complex a -> Stride a

instance Functor Stride where
    fmap f (Stride x y) = Stride (f x) (f y)
    fmap f (Stay x) = Stay (f x)
    fmap f (Complex c) = Stride (f $ start c) (f $ end c)
instance Applicative Stride where
    pure = Stay
    (<*>) (Stride f g) (Stride x y) = Stride (f x) (g y)
    (<*>) (Stride f g) (Stay x) = Stride (f x) (g x)
    (<*>) (Stride f g) (Complex c) = Stride (f $ start c) (g $ end c)
    (<*>) (Stay f) s = fmap f s
    (<*>) (Complex df) (Stride x y) = Stride (start $ df x) (end $ df y)
    (<*>) (Complex df) (Stay x) = df x
    (<*>) (Complex df) (Complex c) = Stride
        (start $ df $ start c)
        (end $ df $ end c)
instance Monad Stride where
    return = Stay
    (>>=) (Stride x y) f = Stride (start $ f x) (end $ f y)
    (>>=) (Stay x) f = f x
    (>>=) (Complex c) f = Stride (start $ f $ start c) (end $ f $ end c)

-- | Type @s@ is a stride for some value type.
class IsStride s where

    -- | Combines two strides where the end of the first is the start of the
    -- second (this condition is not necessarily checked).
    glue :: s -> s -> s

-- | Type @s@ describes the progression of a value of type @a@ between two
-- points ('start' and 'end').
class IsStride s => StrideRel s a | s -> a where

    -- | Gets the value of a stride at the initial point.
    start :: s -> a

    -- | Gets the value of a stride at the final point.
    end :: s -> a

instance IsStride (Stride a) where
    glue (Stride x _) (Stride _ y) = Stride x y
    glue x (Stay _) = x
    glue (Stay _) x = x
    glue (Stride x _) (Complex b) = Stride x (end b)
    glue (Complex a) (Stride _ x) = Stride (start a) x
    glue (Complex a) (Complex b) = Complex (glue a b)
instance StrideRel (Stride a) a where
    start (Stride x _) = x
    start (Stay x) = x
    start (Complex c) = start c
    end (Stride _ x) = x
    end (Stay x) = x
    end (Complex c) = end c

-- | Constructs a stride between two values.
stride :: a -> a -> Stride a
stride = Stride

-- | Constructs a stride with the given value for both points.
stay :: a -> Stride a
stay = Stay

-- | Describes the progression of a value of type @a@ between two points
-- ('start' and 'end') in a way that exposes the relationship between the
-- two points.
type family Complex a

-- Define complex strides for common types.
type instance Complex (a -> b) = a -> Stride b
type instance Complex (a, b) = (Stride a, Stride b)
type instance Complex (a, b, c) = (Stride a, Stride b, Stride c)
instance IsStride (a -> Stride b) where
    glue f g x = glue (f x) (g x)
instance StrideRel (a -> Stride b) (a -> b) where
    start f = start . f
    end f = end . f
instance IsStride (Stride a, Stride b) where
    glue (a, b) (c, d) = (glue a c, glue b d)
instance StrideRel (Stride a, Stride b) (a, b) where
    start (x, y) = (start x, start y)
    end (x, y) = (end x, end y)
instance IsStride (Stride a, Stride b, Stride c) where
    glue (a, b, c) (d, e, f) = (glue a d, glue b e, glue c f)
instance StrideRel (Stride a, Stride b, Stride c) (a, b, c) where
    start (x, y, z) = (start x, start y, start z)
    end (x, y, z) = (end x, end y, end z)

-- | Describes a change that can be applied to values of type @a@.
type Delta a = a -> Stride a

-- | Constructs a delta which, when applied to any value, will result in the
-- given value.
set :: a -> Delta a
set t s = stride s t

-- | A delta which, when applied to any value, will return the same value.
keep :: Delta a
keep = Stay

-- | Applies a delta to a value.
apply :: Delta a -> a -> a
apply delta = end . delta

-- | When given a deltor of a value, returns the stride of the deltor between
-- two predetermined points.
type Evaluator f g = forall a. f a -> g (Stride a)

-- | @f@ is an applicative functor in which values progress over a single
-- dimension (i.e. time). Strides can be used to represent the progression
-- between any two moments in time.
class Applicative f => Deltor f where

    -- | Constructs a deltor using the given defining function, which gets any
    -- stride for the deltor given an evaluator for the target time range.
    deltor :: (forall g. (Applicative g)
        => Evaluator f g -> g (Stride a)) -> f a

instance Deltor Identity where
    deltor def = start <$> def (fmap stay)
instance Deltor Stride where
    deltor def = runIdentity $ def Identity

-- | Insures that the given deltor performs equality checks on values to
-- decide whether they have changed. This may improve performance when applied
-- to deltors of small (easy to compare) values that result from long
-- computations.
dcheck :: (Deltor f, Eq a) => f a -> f a
dcheck source = deltor (\eval -> check <$> eval source) where
    check (Stride x y) | x == y = Stay x
    check (Complex c) | start c == end c = Stay (start c)
    check x = x
