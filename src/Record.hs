{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Record (
    RecordRel (..),
    Record,
    HasRecord,
    map,
    fromList,
    toList,
    elems,
    Empty,
    EnumRecord,
    RecordRel1 (..),
    Record1,
    HasRecord1,
    Void1,
    Empty1
) where

import Prelude hiding (map)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.List as List
import Data.Void
import Data.Foldable hiding (toList)
import Data.Traversable
import Control.Monad.State (execState, modify)
import Control.Monad (void)
import Control.Applicative hiding (empty)

-- | @r a@ is a structured container of items of type @a@ that allows traversal
-- and random access indexed by @n@. Traversal over a record will always yield
-- all items in a consistent order.
class Traversable r => RecordRel (r :: * -> *) n | r -> n where

    -- | Maps the items in a record.
    mapWithName :: (n -> a -> b) -> r a -> r b

    -- | Constructs a record.
    gen :: (n -> a) -> r a

    -- | Gets the value of an item within a record.
    get :: n -> r a -> a

    -- | Sets the value of an item within a record.
    set :: n -> a -> r a -> r a

-- | The prefered record type for name/index type @n@.
type family Record (n :: *) :: * -> *

-- | @n@ is the name/index type for @Record n@.
type HasRecord n = RecordRel (Record n) n

-- | Maps the values in a record.
map :: (RecordRel r n) => (a -> b) -> r a -> r b
map = fmap

-- | Constructs a record from a list. This assumes that every potential item
-- of the record is covered in the list.
fromList :: (RecordRel r n) => [(n, a)] -> r a
fromList = foldl' (\a (n, v) -> set n v a) $
    gen (const $ error "fromList: some names not covered")

-- | Converts a record into a list.
toList :: (RecordRel r n) => r a -> [(n, a)]
toList record = reverse $ execState (
    traverse (\item -> void $ modify (item :)) $
    mapWithName (,) record) []

-- | Gets the elements in a record.
elems :: (RecordRel r n) => r a -> [a]
elems = List.map snd . toList

-- | A record type with no items.
data Empty a = Empty
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
instance RecordRel Empty Void where
    mapWithName _ _ = Empty
    gen _ = Empty
    get = undefined
    set = undefined
type instance Record Void = Empty

-- | A record type indexed by an enum type @e@.
newtype EnumRecord (e :: *) a = EnumRecord (Vector a)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
instance (Bounded e, Enum e) => RecordRel (EnumRecord e) e where
    mapWithName f (EnumRecord vec) = EnumRecord $
        Vector.imap (\i v -> f (toEnum i) v) vec
    gen (f :: e -> a) = EnumRecord (Vector.generate
        (fromEnum (maxBound :: e) - fromEnum (minBound :: e) + 1)
        (f . toEnum))
    get key (EnumRecord vec) = (Vector.!) vec (fromEnum key)
    set key value (EnumRecord vec) = EnumRecord
        ((Vector.//) vec [(fromEnum key, value)])

-- | @r f@ is a structured container of items of type @f a@, where @a@ is
-- determined by the container for each item, that allows traversal and random
-- access indexed by @n a@. Traversal over a record will always yield all items
-- in a consistent order.
class RecordRel1 (r :: (p -> *) -> *) (n :: p -> *) | r -> n, n -> r where

    -- | Traverses a record.
    traverse1 :: (Applicative m)
        => (forall a. f a -> m (g a))
        -> r f -> m (r g)

    -- | Constructs a record.
    gen1 :: (forall a. n a -> f a) -> r f

    -- | Gets the value of an item within a record.
    get1 :: n a -> r f -> f a

    -- | Sets the value of an item within a record.
    set1 :: n a -> f a -> r f -> r f

-- | The prefered record type for name/index type @n@.
type family Record1 (n :: p -> *) :: (p -> *) -> *

-- | @n@ is the name/index type for @Record1 n@.
type HasRecord1 n = RecordRel1 (Record1 n) n

-- | A higher-kinded empty data type.
data Void1 :: p -> *

-- | A record type with no items.
data Empty1 (f :: p -> *) = Empty1
    deriving (Eq, Ord, Show)
instance RecordRel1 Empty1 Void1 where
    traverse1 _ Empty1 = pure Empty1
    gen1 _ = Empty1
    get1 = undefined
    set1 = undefined
type instance Record1 Void1 = Empty1
