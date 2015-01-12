{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
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
    toConTerm,
    AnyTerm (..),
    fromAnyTerm,
    Place,
    substitute,
    substituteInside
) where

import GHC.Exts (Constraint)
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

-- | Constraint implied by @IsTypeList a@
type family TypeListImpl (a :: [Type]) :: Constraint where
    TypeListImpl '[] = ()
    TypeListImpl (a ': b) = (IsType a, IsTypeList b)

-- | @a@ is a valid list of 'Type's.
class (TypeListImpl a, Typeable a) => IsTypeList (a :: [Type]) where

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
    App :: (IsTypeList a) => p a r -> FList (Term p m) a -> Term p m r

-- | Tries converting a term into a concrete term.
toConTerm :: Term p m r -> Maybe (Term p Con r)
toConTerm Slot = Nothing
toConTerm (App prim list) = App prim <$> toConList list where
    toConList :: FList (Term p m) r -> Maybe (FList (Term p Con) r)
    toConList FNil = Just FNil
    toConList (FCons a b) = FCons <$> toConTerm a <*> toConList b

-- | A term of any result type.
data AnyTerm p m = forall r. (IsType r) => AnyTerm (Term p m r)

-- | Tries giving an 'AnyTerm' a specific result type.
fromAnyTerm :: forall p m r. (IsType r) => AnyTerm p m -> Maybe (Term p m r)
fromAnyTerm (AnyTerm inner) = withInner inner where
    withInner :: forall p m q. (IsType q) => Term p m q -> Maybe (Term p m r)
    withInner inner = case eqT :: Maybe (r :~: q) of
        Just Refl -> Just inner
        Nothing -> Nothing

-- | Identifies a place in a 'Term' where substitutions may be possible. All
-- possible substitutions for a term can be described by a place and a target
-- term, however, not all places and target terms correspond to a valid
-- substitution.
--
-- Places are implemented as a list of argument indices. To demonstrate:
-- * @[]@ corresponds to the entire term.
-- * @[0]@ corresponds to the first argument (assuming the term is 'App').
-- * @[1, 0]@ corresponds to the first argument of the second argument.
type Place = [Int]

-- | Tries substituting a sub-term within a term, returning 'Nothing' if the
-- place does not exist or there is a type mismatch.
substitute :: forall p m.
    AnyTerm p m -- ^ base term to substitute
    -> Place -- ^ place to substitute in
    -> AnyTerm p m -- ^ term to substitute in
    -> Maybe (AnyTerm p m)
substitute _ [] target = Just target
substitute (AnyTerm Slot) (_ : _) _ = Nothing
substitute (AnyTerm (App prim args)) (i : is) target = res where
    substituteArg :: forall a. (IsTypeList a)
        => FList (Term p m) a -> Int -> Maybe (FList (Term p m) a)
    substituteArg FNil _ = Nothing
    substituteArg (FCons arg rem) 0 = do
        nArg <- substitute (AnyTerm arg) is target
        nArg <- fromAnyTerm nArg
        return $ FCons nArg rem
    substituteArg (FCons arg rem) n = FCons arg <$> substituteArg rem (n - 1)
    res = (AnyTerm . App prim) <$> substituteArg args i

-- | Like 'substitute', but disallows substitutions of the whole term.
substituteInside :: forall p m r. (IsType r)
    => Term p m r -> Place
    -> AnyTerm p m -> Maybe (Term p m r)
substituteInside t p x = substitute (AnyTerm t) p x >>= fromAnyTerm
