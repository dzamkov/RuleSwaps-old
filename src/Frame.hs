{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Frame (
    DistState (..),
    PeerState,
    FrameAction (..),
    Frame,
    runFrame,
    interactFrame
) where

import Rand (evalRand)
import Game hiding (Read)
import Key (Key)
import qualified Key
import Data.Map (Map)
import qualified Data.List as List
import qualified Data.Map as Map

-- This module contains types and functions needed to run a game over a
-- distributed/P2P network. Messaging code is not included.

-- | Contains state information about a distributed network between frames.
data DistState = DistState {

    -- | Contains all known player keys.
    keys :: Map Player Key }

-- | Contains the full known state information for a network peer.
type PeerState p = (GameState p, PrivateState p, DistState)

-- |  Identifies a primitive action in a frame.
data FrameAction p m a where

    -- | Gets the current peer state.
    Read :: FrameAction p m (PeerState p)

    -- | Writes the current peer state.
    Write :: PeerState p -> FrameAction p m ()

    -- | Given an incomplete mapping of players to values, this will exchange
    -- the mapping with peers until a complete mapping can be established.
    -- There is only one potential value for each player. If conflicting values
    -- are received for a player, a desync will occur.
    Exchange :: Map Player a -> FrameAction p m (Map Player a)

    -- | Requests that at least the given players make the given choice. The
    -- decisions are returned, along with a procedure which will "submit" the
    -- final decisions for all players, once known. The latter feature can
    -- be used for verification.
    Ask :: PlayerChoice p a -> Map Player ()
        -> FrameAction p m (Map Player a, m (Map Player a))

-- | A procedure to be run on a distributed network.
data Frame p a = Frame { runFrame :: forall m. (Monad m) =>
    (forall b. FrameAction p m b -> m b) -> m a }

-- | Creates a frame for a 'GameInteract' action. Returns the result of the
-- action and a 'Key' for the frame's hash, which should be same across all
-- peers, otherwise, a desync has occured.
interactFrame :: GameInteract p a -> Frame p (a, Key)
interactFrame (Draw target amount) = Frame $ \act -> do
    (gameState, privateState, distState) <- act Read
    let initRand = Map.mapWithKey (\player key ->
          if player == target
            then Nothing
            else Just $ Key.hash key) $ keys distState
    resRand <- act $ Exchange initRand
    let randTotal = Map.foldl (\accum cur ->
          maybe accum (Key.merge accum) cur) Key.zero resRand
    let targetKey = Map.lookup target $ keys distState
    let nNextObj = nextObj gameState + fromInteger amount
    let res = [nextObj gameState .. nNextObj - 1]
    let nPrivateState =
          case (targetKey, Map.lookup target $ curDecks privateState) of
            (Just key, Just deck) ->
                let rDeck = replDeck $ getPlayer target gameState
                in let draw = deckDraw rDeck amount deck
                in let randStream = Key.toRandStream (Key.merge key randTotal)
                in let (nDeck, cards) = evalRand draw randStream
                in privateState {
                    curDecks = Map.insert target nDeck $ curDecks privateState,
                    cardInfo = Map.union (cardInfo privateState) $
                        Map.fromList $ List.zip res cards }
            _ -> privateState
    let frameHash = Key.hashObj $ Map.elems resRand
    let nKeys = Map.map (Key.merge frameHash) $ keys distState
    act $ Write (
        gameState { nextObj = nNextObj },
        nPrivateState,
        distState { keys = nKeys })
    return (res, frameHash)
interactFrame _ = error "Frame not implemented"
