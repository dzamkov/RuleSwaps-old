{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Markup.Builder (
    Builder,
    use,
    runBuilder
) where

import Markup
import Unsafe.Coerce
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Monoid
import Control.Applicative
import Control.Monad.Fix

-- | A value in a 'Plex'. 'unsafeCoerce' is used to later retrieve the value.
data Value = forall a. Value a

-- | Combines multiple 'Value's together into a single object.
type Plex = IntMap Value

-- | Constructs a 'Plex' that has a single value set.
setBin :: Int -> Value -> Plex
setBin = IntMap.singleton

-- | Retrieves a value in a 'Plex'.
getBin :: Plex -> Int -> Maybe Value
getBin = flip IntMap.lookup

-- | Allows the construction of composite widgets whose child widgets may
-- be mutually dependent on each other's instance values.
data Builder u a = Builder Int ((Plex -> u) -> Int -> Plex -> a)
instance Functor (Builder u) where
    fmap f (Builder size apply) = Builder size (\raise offset plex ->
        f (apply raise offset plex))
instance Applicative (Builder u) where
    pure x = Builder 0 (\_ _ _ -> x)
    (<*>) (Builder xSize xApply) (Builder ySize yApply) =
        Builder (xSize + ySize) (\raise offset plex ->
            let xValue = xApply raise offset plex
                yValue = yApply raise (offset + xSize) plex
            in xValue yValue)
instance Monad (Builder u) where -- Not really, but do notation is too good
    return = pure
    (>>=) (Builder xSize xApply) f =
        let ySize = getSize f
        in Builder (xSize + ySize) (\raise offset plex ->
            let xValue = xApply raise offset plex
                Builder _ yApply = f xValue
            in yApply raise (offset + xSize) plex)
instance MonadFix (Builder u) where
    mfix f =
        let size = getSize f
        in Builder size (\raise offset plex ->
            let value = apply raise offset plex
                Builder _ apply = f value
            in value)

-- | Gets the size of the builder produced by the given function. This is
-- undefined if the size depends on the argument.
getSize :: (a -> Builder u b) -> Int
getSize f =
    let initArg = error "Builder must have uniform control flow"
        Builder size _ = f initArg
    in size

-- | Brings an instance of a widget into a builder.
use :: (Widget w) => w a -> Builder u (w u, a)
use widget = Builder 1 (\raise offset plex ->
    (fmap (raise . setBin offset . Value) widget,
    case getBin plex offset of
        Just (Value value) -> unsafeCoerce value
        Nothing -> error $ "'use'd widget not present in final widget, " ++
            "no value available"))

-- | Runs a builder to produce a widget.
runBuilder :: (Widget w) => (forall u. (Monoid u) => Builder u (w u, a)) -> w a
runBuilder (Builder _ apply) = res where
    getWidget = fst . apply id 0
    getValue = snd . apply id 0
    widget = wfix getWidget
    res = fmap getValue widget
