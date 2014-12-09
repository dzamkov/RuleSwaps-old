{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Game (
    Player,
    Type (..),
    IsGameType (..),
    Value,
    mkValue,
    getValue,
    Primitive (..),
    Term (..),
    termType,
    Game,
    draw,
    drawN,
    bank,
    roll,
    context
) where

import Data.Typeable (Typeable, cast)
import Control.Monad
import Control.Applicative
import Data.Maybe (fromMaybe)
import qualified Data.List as List

-- | Identifies a player in a game where game objects are identified by
-- a type @h@.
newtype Player h = Player h deriving (Eq, Show, Typeable)

-- | Identifies a type/category of value within the game.
data Type
    = TAction
    | TCondition
    | TPlayer
    | TNumber deriving (Eq, Ord, Show)

-- | Type @a@ is a game type.
class Typeable a => IsGameType a where

    -- | Gets the game type of the type represented by the given value.
    gameTypeOf :: a -> Type

-- Define game types.
instance IsGameType () where
    gameTypeOf _ = TAction
instance IsGameType Bool where
    gameTypeOf _ = TCondition
instance Typeable h => IsGameType (Player h) where
    gameTypeOf _ = TPlayer
instance IsGameType Integer where
    gameTypeOf _ = TNumber

-- | Stores a value of some game type.
data Value where
    Value :: (IsGameType a) => a -> Value

-- | Puts a value of a game type into a 'Value' container.
mkValue :: (IsGameType a) => a -> Value
mkValue = Value

-- | Gets a typed value from a 'Value' container. Fails horribly if the type
-- of the contained value does not match the expected type.
getValue :: (IsGameType a) => Value -> a
getValue (Value x) = fromMaybe (error "Invalid value conversion") (cast x)

-- | Describes a composition of primitive operations of type @p@ that may
-- result in a value.
data Term p h where

    -- | An unfilled slot in the term.
    Slot :: h -> Type -> Term p h

    -- | Applies a primitive operation to a list of argument terms that fill
    -- its slots.
    App :: h -> p -> [Term p h] -> Term p h
    deriving (Eq, Ord, Show)

-- | Describes a procedure/action in game logic that produces a result of type
-- @a@, assuming that @p@ encodes primitive operations and game objects are
-- identified by type @h@.
data Game p h a where

    -- | The given player draws from their deck. Returns the identifier for
    -- the drawn card.
    Draw :: Player h -> Game p h h

    -- | The given player gains the given number of coins (or loses, when
    -- negative).
    Bank :: Player h -> Integer -> Game p h ()

    -- | Performs a public random roll for a natural number less than the
    -- given number.
    Roll :: Integer -> Game p h Integer

    -- | Gives context to a game action by associating it with a slot, rule,
    -- or card. This is purely for user feedback.
    Context :: h -> Game p h a -> Game p h a

    -- | Performs a game action, then uses its result to select another game
    -- action to perform.
    Bind :: Game p h b -> (b -> Game p h a) -> Game p h a

    -- | Returns the value @a@ without doing anything.
    Return :: a -> Game p h a

instance Functor (Game p h) where
    fmap = liftM
instance Applicative (Game p h) where
    pure = Return
    (<*>) = ap
instance Monad (Game p h) where
    (>>=) = Bind
    return = Return

-- | Type @p@ represents a primitive operation that results in a game value.
class Primitive p where

    -- | Gets the argument types and result type for a primitive.
    primitiveType :: p -> ([Type], Type)

    -- | Runs a primitive as a game action using the given game actions as
    -- arguments.
    runPrimitive :: (Typeable h) => p -> [Game p h Value] -> Game p h Value

-- | Gets the slot types and result type for a term.
termType :: (Primitive p, Typeable h) => Term p h -> ([Type], Type)
termType (Slot _ t) = ([t], t)
termType (App _ p args) = (slotTypes, resultType) where
    (_, resultType) = primitiveType p
    slotTypes = List.concatMap (fst . termType) args

-- | The given player draws a card. Returns the identifier for the drawn card.
draw :: Player h -> Game p h h
draw = Draw

-- | The given player draws the given number of cards.
drawN :: Player h -> Integer -> Game p h ()
drawN _ 0 = return ()
drawN p 1 = void (draw p)
drawN p n = draw p >>= (\_ -> drawN p (n - 1))

-- | The given player gains (or loses) the given number of coins.
bank :: Player h -> Integer -> Game p h ()
bank = Bank

-- | Performs a public random roll for a natural number less than the given
-- number.
roll :: Integer -> Game p h Integer
roll = Roll

-- | Gives context to a game action by associating it with a slow, rule,
-- or card. This is purely for user feedback.
context :: h -> Game p h a -> Game p h a
context = Context
