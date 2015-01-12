{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game where

import Prelude hiding (interact)
import Rand
import Base hiding (Value)
import qualified Base
import Data.Monoid
import Control.Monad.Operational
import Control.Applicative

-- | Describes a part of a message.
data MessagePart g = MessageText String | MessagePlayer (Player g)

-- | A message that can be displayed to a player in various circumstances.
newtype Message g = Message [MessagePart g]
    deriving (Monoid)

-- | Converts a string into a message.
msg :: String -> Message g
msg text = Message [MessageText text]

-- | Creates a message consisting of a player.
msgPlayer :: Player g -> Message g
msgPlayer player = Message [MessagePlayer player]

-- | Creates a message specifying the quantity of something. The singular
-- and plural forms of that something should be given.
msgQty :: String -> String -> Integer -> Message g
msgQty _ plural 0 = msg $ "no " ++ plural
msgQty singular _ 1 = msg $ "a " ++ singular
msgQty _ plural n = msg $ show n ++ " " ++ plural

-- | Creates a message specifying a quantity of coins.
msgCoins :: Integer -> Message g
msgCoins = msgQty "coin" "coins"

-- | Creates a message specifying a quantity of cards.
msgCards :: Integer -> Message g
msgCards = msgQty "card" "cards"

-- | Concatenates messages with a space.
infixr 6 <~>
(<~>) :: Message g -> Message g -> Message g
(<~>) x y = x <> msg " " <> y

-- | Identifies a player within the game context @g@.
type family Player g :: *

-- | Identifies a card within the game context @g@.
type family Card g :: *

-- | Identifies a deck within the game context @g@.
type family Deck g :: *

-- | Identifies a consitutional rule within the game context @g@.
type family Rule g :: *

-- | Identifies a primitive operation within the game context @g@.
type family Prim g :: [Type] -> Type -> *

-- | Identifies a value within the game context @g@.
type Value g = Base.Value (Player g)

-- | Describes a card within the game context @g@.
type CardInfo g = AnyTerm (Prim g) Abs

-- | Describes a deck within the game context @g@ by giving information for
-- the unique cards in the deck, along with their multiplicites.
type DeckInfo g = [(Integer, CardInfo g)]

-- | A primitive instruction for a game, resulting in a value of type @a@.
data GameInstr g a where

    -- | Performs a query for public information.
    Query :: GameQuery g a -> GameInstr g a

    -- | Performs an effect without interaction.
    Effect :: GameEffect g a -> GameInstr g a

    -- | Performs an interactive operation.
    Interact :: GameInteract g a -> GameInstr g a

-- | An instruction for a game that obtains public information.
data GameQuery g a where

    -- | Gets the set of players in the game, ordered by turn order. The first
    -- player is the current player, the next is the one afterwards, and so
    -- on.
    GetPlayers :: GameQuery g [Player g]

    -- | Gets the deck for the given player.
    GetDeck :: Player g -> GameQuery g (Deck g)

    -- | Gets the set of cards the given player has.
    GetHand :: Player g -> GameQuery g (Card g)

    -- | Gets the number of coins the given player has.
    GetWealth :: Player g -> GameQuery g Integer

-- | An instruction for a game which modifies public information.
data GameEffect g a where

    -- | Adds the given number of coins to the given player's wealth. Wealth
    -- should not go below zero (not checked).
    Bank :: Player g -> Integer -> GameEffect g ()

    -- | Removes a card from the hand of the player holding it. The card should
    -- be held by someone (not checked).
    Discard :: Card g -> GameEffect g ()

    -- | Adds a card to a player's hand. The card should not be held by
    -- anyone (not checked).
    Give :: Card g -> GameEffect g ()

    -- | Creates a card with the given specification.
    MaterializeCard :: CardInfo g -> GameEffect g (Card g)

    -- | Creates a deck with the given specification.
    MaterializeDeck :: DeckInfo g -> GameEffect g (Deck g)

    -- | Displays a message to all players.
    Print :: Message g -> GameEffect g ()

-- | An instruction for a game that requires interaction or non-public
-- information to perform.
data GameInteract g a where

    -- | Players simultaneously make a decision.
    Choice :: (Monoid a) => (Player g -> PlayerChoice g a) -> GameInteract g a

    -- | Draws a card from the given deck.
    Draw :: Deck g -> GameInteract g (Card g)

    -- | The holder of the given card must reveal it to the given player.
    Reveal :: Card g -> Player g -> GameInteract g ()

    -- | The holder of the given card must reveal it to all other players.
    RevealAll :: Card g -> GameInteract g (CardInfo g)

    -- | Publicly choose a random value.
    Rand :: Rand a -> GameInteract g a

    -- | Performs a synchronized pause mediated by the given player.
    Pause :: Player g -> GameInteract g ()

-- | Identifies a procedure within the context of a game.
newtype Game g a = Game (Program (GameInstr g) a)
    deriving (Functor, Applicative, Monad)

-- | Constructs a game procedure from a single query.
query :: GameQuery g a -> Game g a
query = Game . singleton . Query

-- | Gets the number of coins the given player currently has.
getWealth :: Player g -> Game g Integer
getWealth = query . GetWealth

-- | Constructs a game procedure from a single effect.
effect :: GameEffect g a -> Game g a
effect = Game . singleton . Effect

-- | Adds the given number of coins to the given player's wealth. Wealth should
-- not fall below zero (not checked).
bank :: Player g -> Integer -> Game g ()
bank player amount = effect $ Bank player amount

-- | Displays a message to all players.
print :: Message g -> Game g ()
print = effect . Print

-- | Constructs a game procedure from a single interactive action.
interact :: GameInteract g a -> Game g a
interact = Game . singleton . Interact

-- | Performs a synchronized pause mediated by the given player.
pause :: Player g -> Game g ()
pause = interact . Pause

-- | Identifies an individual decision that an individual player can make,
-- resulting in a value of type @a@.
data PlayerChoiceInstr g a where

    -- | The player can make a choice to continue to the next section of the
    -- decision tree, or not. This is like 'ChooseYesNo', but is implicitly
    -- yes. The given message is shown in regards to "cancelling" to make
    -- this no.
    ChooseContinue :: Message g -> PlayerChoiceInstr g Bool

    -- | The player makes a yes or no decision.
    ChooseYesNo :: Message g -> PlayerChoiceInstr g Bool

    -- | The player chooses another player in the given set.
    ChoosePlayer :: Message g
        -> (Player g -> Maybe a)
        -> PlayerChoiceInstr g a

    -- | The player chooses a number in the given range (inclusive).
    ChooseNumber :: Message g -> Integer -> Integer
        -> PlayerChoiceInstr g Integer

    -- | The player chooses a card within the given set.
    ChooseCard :: Message g
        -> (forall r. (IsType r) => Card g -> Term (Prim g) Abs r -> Maybe a)
        -> PlayerChoiceInstr g a

    -- | The player chooses a rule in the given set.
    ChooseRule :: Message g
        -> (Rule g -> Maybe a)
        -> PlayerChoiceInstr g a

-- | Identifies a decision that an individual player can make, resulting in
-- a value of type @a@.
newtype PlayerChoice g a = PlayerChoice (Program (PlayerChoiceInstr g) a)
    deriving (Functor, Applicative, Monad)
