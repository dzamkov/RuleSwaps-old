{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
module Actor.Proto (
    Proto,
    yield,
    sendP,
    spawnP
) where

import Actor.Internal
import Control.Applicative

-- | Describes a protocol for back-and-forth communication between actors. The
-- list of types for the protocol correspond to the types of messages expected
-- at various points, with the first type being the type of the message needed
-- to initiate the protocol.
data family Proto r (k :: [*])
data instance Proto r '[] = EmptyProto
data instance Proto r (a ': s) = Proto (a -> Actor r (ProtoSource r s))

-- | A source that responds to a protocol.
type family ProtoSource r (k :: [*])
type instance ProtoSource r '[] = ()
type instance ProtoSource r (a ': s) = Source r (a, Proto r s)

-- | Initiates a protocol, sending the initial message and waiting for the
-- response.
yield :: (MonadActor r m) => Proto r (a ': b ': s) -> a -> m (b, Proto r s)
yield (Proto proto) msg = do
    source <- liftActor $ proto msg
    await source

-- | Sends the final message of a protocol.
sendP :: (MonadActor r m) => Proto r (a ': '[]) -> a -> m ()
sendP (Proto proto) msg = liftActor $ proto msg

-- | @s@ is a valid sequence of message types for which a protocol can be
-- spawned.
class ProtoChain (s :: [*]) where

    -- | Constructs a protocol along with an associated event source.
    spawnP :: (MonadActor r m) => m (Proto r (a ': s), Source r (a, Proto r s))

instance ProtoChain '[] where
    spawnP = do
        chan <- spawn
        return (Proto $ send chan, (, EmptyProto) <$> source chan)
instance ProtoChain s => ProtoChain (a ': s) where
    spawnP = do
        chan <- spawn
        let init msg = do
            (responseP, responseS) <- spawnP
            send chan (msg, responseP)
            return responseS
        return (Proto init, source chan)
