{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
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
    Deck,
    deckSize,
    deckTake,
    deckDraw,
    termType,
    PlayerState (..),
    GameState (..),
    getPlayer,
    PrivateState (..),
    getCard,
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
    rand,
    pause,
    message,
    context
) where

import Rand (Rand, roll)
import Data.Typeable (Typeable, cast)
import Control.Monad
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List

-- | Identifies a game object.
type Object = Int

-- | Identifies a player in a game.
newtype Player = Player Object deriving (Eq, Show, Typeable)

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
instance IsGameType Player where
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
-- result in a value. Values of type @h@ are used to sub-expressions in the
-- term.
data Term p h where

    -- | An unfilled slot in the term.
    Slot :: Type -> Term p h

    -- | Applies a primitive operation to a list of argument terms that fill
    -- its slots.
    App :: p -> [(h, Term p h)] -> Term p h
    deriving (Eq, Ord, Show)

-- | Describes a possible card.
type Card p = Term p ()

-- | Describes a possible deck.
type Deck p = [(Card p, Integer)]

-- | Gets the total number of cards in a deck.
deckSize :: Deck p -> Integer
deckSize = List.sum . List.map snd

-- | Takes the nth card from a deck.
deckTake :: Deck p -> Integer -> (Deck p, Card p)
deckTake deck i = (nDeck, card) where
    (nDeck, Right card) = List.foldl (\accum item -> case (accum, item) of
        ((nDeck, Right card), item) -> (item : nDeck, Right card)
        ((nDeck, Left i), (card, mult)) | i < mult ->
            ((card, mult - 1) : nDeck, Right card)
        ((nDeck, Left i), item@(_, mult)) ->
            (item : nDeck, Left (i - mult))) ([], Left i) deck

-- | Randomly draws a number of cards from a deck. A replacement deck must be
-- given in the event that the deck runs out of cards.
deckDraw :: Deck p -> Integer -> Deck p -> Rand (Deck p, [Card p])
deckDraw repl n cur = go size n cur [] where
    size = deckSize cur
    repln = deckSize repl
    go 0 n _ accum = go repln n repl accum
    go _ 0 cur accum = return (cur, List.reverse accum)
    go size n cur accum = do
        i <- roll size
        let (nCur, card) = deckTake cur i
        go (size - 1) (n - 1) nCur (card : accum)

-- | Contains all publicly-available information about a player in a game.
data PlayerState = PlayerState {

    -- | The name of the player.
    name :: String,

    -- | The amount of coins the player has.
    coins :: Integer,

    -- | The set of cards the player is holding.
    cards :: [Object] }

-- | Contains all publicly-available information about a game at a particular
-- moment between actions.
data GameState p = GameState {

    -- | The set of players in the game in turn order. The current player
    -- is first, the next player is next, and so on.
    players :: [(Player, PlayerState)],

    -- | The current game constitution.
    constitution :: [(Object, Term p Object)],

    -- | The next term in the consitution to be executed.
    place :: Object,

    -- | The identifier to be assigned to the next created object.
    nextObj :: Object }

-- | Gets the state of a player in a 'GameState', assuming the player is in
-- the game.
getPlayer :: Player -> GameState p -> PlayerState
getPlayer target state = snd $ fromMaybe (error "Player does not exist") $
    List.find ((== target) . fst) $ players state

-- | Contains game information that a player keeps to themselves.
data PrivateState p = PrivateState {

    -- | Contains the identities of all known cards.
    cardInfo :: Map Object (Card p) }

-- | Gets the identity of the given card, or return 'Nothing' if the card is
-- unknown.
getCard :: Object -> PrivateState p -> Maybe (Card p)
getCard id = Map.lookup id . cardInfo

-- | Identifies a decision that an individual player can make, resulting in
-- a value of type @a@.
data PlayerChoice p a where

    -- | The player makes a yes or no decision.
    YesNo :: PlayerChoice p Bool

    -- | The player choses a number up to their current coin count. The
    -- parameter is a hint for the player which tells whether the coins will
    -- be unconditionally lost (as in a payment).
    Coin :: Bool -> PlayerChoice p Integer

    -- | A composite player choice made up from responses of two choices.
    Multi :: PlayerChoice p a -> PlayerChoice p b -> PlayerChoice p (a, b)

-- | Identifies a primitive action in a game that modifies or uses non-public
-- information. These require coordination between players somehow.
data GameInteract p a where

    -- | The given player draws the given amount of cards from their deck.
    -- Returns the identifiers for the drawn cards.
    Draw :: Player -> Integer -> GameInteract p [Object]

    -- | The given player has to make a choice.
    Choice :: Player -> PlayerChoice p a -> GameInteract p a

    -- | All players have to make a choice.
    ChoiceAll :: PlayerChoice p a -> GameInteract p (Map Player a)

    -- | A public random value is generated.
    Rand :: Rand a -> GameInteract p a

    -- | The game is paused until a human decides to continue it, giving the
    -- humans a chance to see what is happening.
    Pause :: GameInteract p ()

-- | Identifies a simple action in a game that modifies only public
-- information.
data GameEffect p a where

    -- | The given player gains the given number of coins (or loses, when
    -- negative). If this is negative, it may not be more coins than the plaer
    -- has.
    Bank :: Player -> Integer -> GameEffect p ()

-- | Identifies a primitive action in a game.
data GameAction p a where

    -- | An action that requires coordination between players.
    Interact :: GameInteract p a -> GameAction p a

    -- | An action that can be described as a modification of publicly-
    -- available information.
    Effect :: GameEffect p a -> GameAction p a

    -- | Gets the current game state.
    Read :: GameAction p (GameState p)

    -- | Displays a message to the players.
    Message :: String -> GameAction p ()

    -- | Performs the given action within the context of a rule, slot or card.
    -- This is purely for user feedback.
    Context :: Object -> Game p a -> GameAction p a

-- | Describes a complex procedure/action in game logic that produces a result
-- of type @a@, assuming that @p@ encodes primitive operations.
data Game p a = Game { runGame :: (Monad m)
    => (forall b. GameAction p b -> m b) -> m a }

instance Functor (Game p) where
    fmap = liftM
instance Applicative (Game p) where
    pure = return
    (<*>) = ap
instance Monad (Game p) where
    (>>=) game f = Game { runGame = \act ->
        runGame game act >>= \res -> runGame (f res) act }
    return x = Game { runGame = \_ -> return x }

-- | Type @p@ represents a primitive operation that results in a game value.
class Primitive p where

    -- | Gets the argument types and result type for a primitive.
    primitiveType :: p -> ([Type], Type)

    -- | Runs a primitive as a game using the given game as arguments.
    runPrimitive :: p -> [Game p Value] -> Game p Value

-- | Gets the slot types and result type for a term.
termType :: (Primitive p) => Term p h -> ([Type], Type)
termType (Slot t) = ([t], t)
termType (App p args) = (slotTypes, resultType) where
    (_, resultType) = primitiveType p
    slotTypes = List.concatMap (\(_, a) -> fst $ termType a) args

-- | Constructs a game for a 'GameAction'.
action :: GameAction p a -> Game p a
action i = Game { runGame = \act -> act i }

-- | Constructs a game from a 'GameInteract'.
interaction :: GameInteract p a -> Game p a
interaction = action . Interact

-- | constructs a game from a 'GameEffect'.
effect :: GameEffect p a -> Game p a
effect = action . Effect

-- | Displays a message to the players.
message :: String -> Game p ()
message = action . Message

-- | Gives context to a game action by associating it with a slot, rule,
-- or card. This is purely for user feedback.
context :: Object -> Game p a -> Game p a
context obj = action . Context obj

-- | Gets the current game state.
state :: Game p (GameState p)
state = action Read

-- | The given player draws a card. Returns the identifier for the drawn card.
draw :: Player -> Game p Object
draw p = head <$> interaction (Draw p 1)

-- | The given player draws the given number of cards.
drawN :: Player -> Integer -> Game p [Object]
drawN p n = interaction (Draw p n)

-- | The given player gains the given number of coins.
gainCoins :: Player -> Integer -> Game p ()
gainCoins p n = effect (Bank p n)

-- | The given player loses the given number of coins.
loseCoins :: Player -> Integer -> Game p ()
loseCoins p n = effect (Bank p (-n))

-- | Gets a public random value.
rand :: Rand a -> Game p a
rand = interaction . Rand

-- | Pauses the game for a human player to continue it.
pause :: Game p ()
pause = interaction Pause
