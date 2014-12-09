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
    PlayerState (..),
    GameState (..),
    GameAction (..),
    Game,
    action,
    state,
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
    Slot :: Type -> Term p h

    -- | Applies a primitive operation to a list of argument terms that fill
    -- its slots.
    App :: p -> [(h, Term p h)] -> Term p h
    deriving (Eq, Ord, Show)

-- | Contains all publicly-available information about a player in a game.
data PlayerState h = PlayerState {

    -- | The amount of coins the player has.
    coins :: Integer,

    -- | The set of cards the player is holding.
    cards :: [h] }

-- | Contains all publicly-available information about a game at a particular
-- moment between actions.
data GameState p h = GameState {

    -- | The set of players in the game in turn order. The current player
    -- is first, the next player is next, and so on.
    players :: [(Player h, PlayerState h)],

    -- | The current game constitution.
    constitution :: [(h, Term p h)],

    -- | The next term in the consitution to be executed.
    place :: h }

-- | Identifies a primitive action in a game procedure. See 'Game'.
data GameAction p h a where

    -- | The given player draws from their deck. Returns the identifier for
    -- the drawn card.
    Draw :: Player h -> GameAction p h h

    -- | The given player gains the given number of coins (or loses, when
    -- negative).
    Bank :: Player h -> Integer -> GameAction p h ()

    -- | Performs a public random roll for a natural number less than the
    -- given number.
    Roll :: Integer -> GameAction p h Integer

-- | Describes a complex procedure/action in game logic that produces a result
-- of type @a@, assuming that @p@ encodes primitive operations and game objects
-- are identified by type @h@.
data Game p h a where

    -- | Gives context to a game by associating it with a slot, rule, or card.
    -- This is purely for user feedback.
    Context :: h -> Game p h a -> Game p h a

    -- | Returns a value without doing anything.
    Return :: a -> Game p h a

    -- | Gets the current game state, and performs a game according to it.
    Read :: (GameState p h -> Game p h a) -> Game p h a

    -- | Given the current 'GameState', returns the action to be performed
    -- along with the continuation of the game, after the action is resolved.
    Cont :: GameAction p h b -> (b -> Game p h a) -> Game p h a

instance Functor (Game p h) where
    fmap = liftM
instance Applicative (Game p h) where
    pure = Return
    (<*>) = ap
instance Monad (Game p h) where
    (>>=) (Context h inner) f = Context h (inner >>= f)
    (>>=) (Return x) f = f x
    (>>=) (Read g) f = Read (g >=> f)
    (>>=) (Cont act cont) f = Cont act (cont >=> f)
    return = Return

-- | Type @p@ represents a primitive operation that results in a game value.
class Primitive p where

    -- | Gets the argument types and result type for a primitive.
    primitiveType :: p -> ([Type], Type)

    -- | Runs a primitive as a game using the given game as arguments.
    runPrimitive :: (Typeable h) => p -> [Game p h Value] -> Game p h Value

-- | Gets the slot types and result type for a term.
termType :: (Primitive p, Typeable h) => Term p h -> ([Type], Type)
termType (Slot t) = ([t], t)
termType (App p args) = (slotTypes, resultType) where
    (_, resultType) = primitiveType p
    slotTypes = List.concatMap (\(_, a) -> fst $ termType a) args

-- | constructs a game from a game action.
action :: GameAction p h a -> Game p h a
action act = Cont act Return

-- | Gets the current game state.
state :: Game p h (GameState p h)
state = Read Return

-- | The given player draws a card. Returns the identifier for the drawn card.
draw :: Player h -> Game p h h
draw p = action (Draw p)

-- | The given player draws the given number of cards.
drawN :: Player h -> Integer -> Game p h ()
drawN _ 0 = return ()
drawN p 1 = void (draw p)
drawN p n = draw p >>= (\_ -> drawN p (n - 1))

-- | The given player gains (or loses) the given number of coins.
bank :: Player h -> Integer -> Game p h ()
bank p n = action (Bank p n)

-- | Performs a public random roll for a natural number less than the given
-- number.
roll :: Integer -> Game p h Integer
roll n = action (Roll n)

-- | Gives context to a game action by associating it with a slow, rule,
-- or card. This is purely for user feedback.
context :: h -> Game p h a -> Game p h a
context = Context
