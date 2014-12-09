{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Game (
    Player,
    Type (..),
    IsGameType (..),
    Value,
    mkValue,
    actValue,
    getValue,
    Primitive (..),
    Term (..),
    Card,
    termType,
    PlayerState (..),
    GameState (..),
    getPlayer,
    PlayerChoice (..),
    GameInteract (..),
    GameEffect (..),
    GameAction (..),
    Game,
    interaction,
    effect,
    state,
    draw,
    drawN,
    gainCoins,
    loseCoins,
    roll,
    pause,
    message,
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

-- | The return value of all actions.
actValue :: Value
actValue = mkValue ()

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

-- | Describes a possible card.
type Card p = Term p ()

-- | Contains all publicly-available information about a player in a game.
data PlayerState h = PlayerState {

    -- | The name of the player.
    name :: String,

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

-- | Gets the state of a player in a 'GameState', assuming the player is in
-- the game.
getPlayer :: (Eq h) => Player h -> GameState p h -> PlayerState h
getPlayer target state = snd $ fromMaybe (error "Player does not exist") $
    List.find ((== target) . fst) $ players state

-- | Identifies a decision that an individual player can make, resulting in
-- a value of type @a@.
data PlayerChoice p h a where

    -- | The player makes a yes or no decision.
    YesNo :: PlayerChoice p h Bool

    -- | The player choses a number up to their current coin count. These
    -- parameter is a hint for the player which tells whether the coins will
    -- be unconditionally lost (as in a payment).
    Coin :: Bool -> PlayerChoice p h Integer

    -- | A composite player choice made up from responses of two choices.
    Multi :: PlayerChoice p h a -> PlayerChoice p h b
        -> PlayerChoice p h (a, b)

-- | Identifies a primitive action in a game that modifies or uses non-public
-- information. These require coordination between players somehow.
data GameInteract p h a where

    -- | The given player draws from their deck. Returns the identifier for
    -- the drawn card.
    Draw :: Player h -> GameInteract p h h

    -- | The given player has to make a choice.
    Choice :: Player h -> PlayerChoice p h a -> GameInteract p h a

    -- | All players have to make a choice.
    ChoiceAll :: PlayerChoice p h a -> GameInteract p h [(Player h, a)]

    -- | Performs a public random roll for a natural number less than the given
    -- number.
    Roll :: Integer -> GameInteract p h Integer

    -- | The game is paused until a human decides to continue it, giving the
    -- human's a chance to see what is happening.
    Pause :: GameInteract p h ()

-- | Identifies a simple action in a game that modifies only public
-- information.
data GameEffect p h a where

    -- | The given player gains the given number of coins (or loses, when
    -- negative). If this is negative, it may not be more coins than the plaer
    -- has.
    Bank :: Player h -> Integer -> GameEffect p h ()

-- | Identifies a primitive action in a game.
data GameAction p h a where

    -- | An action that requires coordination between players.
    Interact :: GameInteract p h a -> GameAction p h a

    -- | An action that can be described as a modification of publicly-
    -- available information.
    Effect :: GameEffect p h a -> GameAction p h a

    -- | Gets the current game state.
    Read :: GameAction p h (GameState p h)

    -- | Displays a message to the players.
    Message :: String -> GameAction p h ()

    -- | Performs the given action within the context of a rule, slot or card.
    -- This is purely for user feedback.
    Context :: h -> Game p h a -> GameAction p h a

-- | Describes a complex procedure/action in game logic that produces a result
-- of type @a@, assuming that @p@ encodes primitive operations and game objects
-- are identified by type @h@.
data Game p h a where

    -- | Returns a value without doing anything.
    Return :: a -> Game p h a

    -- | Requests a 'GameAction' to be performed, and then provides a
    -- contiunation of the game based on the resulting value.
    Cont :: GameAction p h b -> (b -> Game p h a) -> Game p h a

instance Functor (Game p h) where
    fmap = liftM
instance Applicative (Game p h) where
    pure = Return
    (<*>) = ap
instance Monad (Game p h) where
    (>>=) (Return x) f = f x
    (>>=) (Cont act cont) f = Cont act (cont >=> f)
    return = Return

-- | Type @p@ represents a primitive operation that results in a game value.
class Primitive p where

    -- | Gets the argument types and result type for a primitive.
    primitiveType :: p -> ([Type], Type)

    -- | Runs a primitive as a game using the given game as arguments.
    runPrimitive :: (Eq h, Typeable h) => p
        -> [Game p h Value] -> Game p h Value

-- | Gets the slot types and result type for a term.
termType :: (Primitive p, Typeable h) => Term p h -> ([Type], Type)
termType (Slot t) = ([t], t)
termType (App p args) = (slotTypes, resultType) where
    (_, resultType) = primitiveType p
    slotTypes = List.concatMap (\(_, a) -> fst $ termType a) args

-- | Constructs a game from a 'GameInteract'.
interaction :: GameInteract p h a -> Game p h a
interaction i = Cont (Interact i) Return

-- | constructs a game from a 'GameEffect'.
effect :: GameEffect p h a -> Game p h a
effect e = Cont (Effect e) Return

-- | Gets the current game state.
state :: Game p h (GameState p h)
state = Cont Read Return

-- | The given player draws a card. Returns the identifier for the drawn card.
draw :: Player h -> Game p h h
draw p = interaction (Draw p)

-- | The given player draws the given number of cards.
drawN :: Player h -> Integer -> Game p h ()
drawN _ 0 = return ()
drawN p 1 = void (draw p)
drawN p n = draw p >>= (\_ -> drawN p (n - 1))

-- | The given player gains the given number of coins.
gainCoins :: Player h -> Integer -> Game p h ()
gainCoins p n = effect (Bank p n)

-- | The given player loses the given number of coins.
loseCoins :: Player h -> Integer -> Game p h ()
loseCoins p n = effect (Bank p (-n))

-- | Performs a public random roll for a natural number less than the given
-- number.
roll :: Integer -> Game p h Integer
roll n = interaction (Roll n)

-- | Pauses the game for a human player to continue it.
pause :: Game p h ()
pause = interaction Pause

-- | Displays a message to the players.
message :: String -> Game p h ()
message m = Cont (Message m) Return

-- | Gives context to a game action by associating it with a slow, rule,
-- or card. This is purely for user feedback.
context :: h -> Game p h a -> Game p h a
context id inner = Cont (Context id inner) Return
