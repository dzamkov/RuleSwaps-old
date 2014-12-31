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
    checkS,
    funS,
    fstS,
    sndS,
    plex2S,
    break2S
) where

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

-- | Performs an equality check on a stride to determine if 'start' and 'end'
-- are the same, and converts the stride into a form of 'stay' if so.
checkS :: (Eq a) => Stride a -> Stride a
checkS (Stride x y) | x == y = Stay x
checkS (Complex c) | start c == end c = Stay (start c)
checkS x = x

-- | Converts a function to a stride into a stride of a function.
funS :: (a -> Stride b) -> Stride (a -> b)
funS = Complex

-- | Extracts the first element from a stride of a tuple.
fstS :: Stride (a, b) -> Stride a
fstS (Complex (sx, _)) = sx
fstS s = fst <$> s

-- | Extracts the second element from a stride of a tuple.
sndS :: Stride (a, b) -> Stride b
sndS (Complex (_, sy)) = sy
sndS s = snd <$> s

-- | Constructs a stride for a tuple.
plex2S :: Stride a -> Stride b -> Stride (a, b)
plex2S x y = Complex (x, y)

-- | Extracts the elements from a stride of a tuple.
break2S :: Stride (a, b) -> (Stride a, Stride b)
break2S (Complex (sx, sy)) = (sx, sy)
break2S s = (fst <$> s, snd <$> s)
