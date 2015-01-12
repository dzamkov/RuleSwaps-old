{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Base (
    Type (..),
    IsType (..),
    raiseType,
    IsTypeList (..),
    FList (..),
    BaseValue,
    Value (..),
    Manner (..),
    Term (..),
    toConTerm
) where

import Data.Typeable
import Control.Applicative

-- Orphan instances allow type-level lists to be typeable.
deriving instance Typeable '[]
deriving instance Typeable '(:)

-- | Identifies the type of a value within the game.
data Type
    = Action
    | Condition
    | Player
    | Number
    deriving (Eq, Ord, Show, Typeable)
deriving instance Typeable Action
deriving instance Typeable Player
deriving instance Typeable Condition
deriving instance Typeable Number

-- | @a@ is of kind 'Type'.
class Typeable a => IsType (a :: Type) where

    -- | Converts a type-level 'Type' to a value-level 'Type'.
    lowerType :: Proxy a -> Type
instance IsType Action where
    lowerType _ = Action
instance IsType Condition where
    lowerType _ = Condition
instance IsType Player where
    lowerType _ = Player
instance IsType Number where
    lowerType _ = Number

-- | Converts a value-level 'Type' to a type-level 'Type'.
raiseType :: Type -> (forall a. (IsType a) => Proxy a -> r) -> r
raiseType Action inner = inner (Proxy :: Proxy Action)
raiseType Condition inner = inner (Proxy :: Proxy Condition)
raiseType Player inner = inner (Proxy :: Proxy Player)
raiseType Number inner = inner (Proxy :: Proxy Number)

-- | @a@ is a valid list of 'Type's.
class Typeable a => IsTypeList (a :: [Type]) where

    -- | Constructs an 'FList' using this type-list as a template.
    mkTermList :: (Applicative g) => Proxy a
        -> (forall b. (IsType b) => g (f b))
        -> g (FList f a)

instance IsTypeList '[] where
    mkTermList _ _ = pure FNil
instance (IsType a, IsTypeList b) => IsTypeList (a ': b) where
    mkTermList _ inner = FCons <$> inner <*>
        mkTermList (Proxy :: Proxy b) inner

-- | A heterogenously-typed list where the type of each item is found by
-- applying a constant type function to the element type.
data FList (f :: k -> *) (t :: [k]) where
    FNil :: FList f '[]
    FCons :: f a -> FList f b -> FList f (a ': b)

-- | A value of game type @t@, where @p@ is an identifier for a player.
type family BaseValue (p :: *) (t :: Type) where
    BaseValue p Action = ()
    BaseValue p Condition = Bool
    BaseValue p Player = p
    BaseValue p Number = Integer

-- | A value of game type @t@, where @p@ is an identifier for a player.
newtype Value (p :: *) (t :: Type) = Value (BaseValue p t)

-- | Specifies whether a term has slots ('Abs') or not ('Con').
data Manner = Abs | Con

-- | A composition of primitives that yields a value of type @r@. When abstract
-- (@m ~ Abs@), the term may contain "slots" which are placeholders for values.
data Term (p :: [Type] -> Type -> *) (m :: Manner) (r :: Type) where

    -- | A placeholder for a value.
    Slot :: Term p Abs r

    -- | An application of a primitive to a list of terms representing
    -- arguments.
    App :: p a r -> FList (Term p m) a -> Term p m r

-- | Tries converting a term into a concrete term.
toConTerm :: Term p m r -> Maybe (Term p Con r)
toConTerm Slot = Nothing
toConTerm (App prim list) = App prim <$> toConList list where
    toConList :: FList (Term p m) r -> Maybe (FList (Term p Con) r)
    toConList FNil = Just FNil
    toConList (FCons a b) = FCons <$> toConTerm a <*> toConList b
