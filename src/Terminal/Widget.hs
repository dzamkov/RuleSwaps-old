{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
module Terminal.Widget (
    Widget,
    runWidget
) where

import qualified Markup
import Terminal.Context (Terminal)
import Terminal.Flow (Flow)
import Terminal.Block (Block)
import Terminal.Paint (runPaint)
import qualified Terminal.Block as Block
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Control.Arrow (first)

-- | Identifies a key that can be assigned to an action.
type Key = Char

-- | The information needed to instantiate a widget.
data Context t k = Context {

    -- | An event that occurs when an assigned key is pressed.
    key :: Event t k }

-- | The information provided by a running widget.
data Info t k = Info {

    -- | The initial requested keys for the widget. Each identifier is
    -- associated with a list of keys that may be assigned to it, in order
    -- of preference.
    initialKeys :: [(k, [Key])],

    -- | An event that occurs when a key request for the widget changes. Each
    -- event consists of an identifier an a list of keys that may be assigned
    -- to it, in order of preference. This may cause an existing identifier to
    -- be reassigned or cleared (if the key list is empty).
    keyAssign :: Event t (k, [Key]) }

-- | Extends a figure of type @a@ with user interaction capabilities, such as
-- selection and key input. An active instance of the widget produces a value
-- of type @b@.
data Widget t d a = forall k. (Ord k) => Widget
    (Context t k -> Moment t (d (Behavior t), a, Info t k))

-- | Constructs a non-interactive widget with the given value and figure.
toWidget :: a -> d (Behavior t) -> Widget t d a
toWidget value figure = Widget $ const $ pure (figure, value, Info {
    initialKeys = [] :: [((), [Key])],
    keyAssign = never })

-- | Decorates a widget using the given function to decorate the underlying
-- figure.
decorate :: (d (Behavior t) -> e (Behavior t)) -> Widget t d a -> Widget t e a
decorate decorateFigure (Widget x) = Widget $ \context -> do
    (xFig, xValue, xInfo) <- x context
    return (decorateFigure xFig, xValue, xInfo)

-- | Composes two widgets using the given function to compose the underlying
-- figures.
compose :: (Monoid a)
    => (d (Behavior t) -> e (Behavior t) -> f (Behavior t))
    -> Widget t d a -> Widget t e a -> Widget t f a
compose composeFigures (Widget x) (Widget y) = Widget $ \context -> do
    (xFig, xValue, xInfo) <- x Context {
        key = filterJust $ either Just (const Nothing) <$> key context }
    (yFig, yValue, yInfo) <- y Context {
        key = filterJust $ either (const Nothing) Just <$> key context }
    return (composeFigures xFig yFig, xValue <> yValue, Info {
        initialKeys = fmap (first Left) (initialKeys xInfo) ++
            fmap (first Right) (initialKeys yInfo),
        keyAssign = fmap (first Left) (keyAssign xInfo) `union`
            fmap (first Right) (keyAssign yInfo) })

instance Monoid a => Monoid (Widget t Flow a) where
    mempty = toWidget mempty mempty
    mappend = compose mappend
instance Monoid a => Markup.Flow Terminal (Widget t Flow a) where
    weakSpace = toWidget mempty . Markup.weakSpace
    strongSpace = toWidget mempty . Markup.strongSpace
    tightText font fore str = toWidget mempty $ Markup.tightText font fore str
instance Monoid a => Markup.Block Terminal (Widget t Block a) where
    clear = toWidget mempty Markup.clear
    (|||) = compose (Markup.|||)
    (===) = compose (Markup.===)
    over = compose Markup.over
    setWidth width = decorate (Markup.setWidth width)
    setHeight height = decorate (Markup.setHeight height)
    setBack nBack = decorate (Markup.setBack nBack)
instance Monoid a => Markup.FlowToBlock Terminal
    (Widget t Flow a) (Widget t Block a) where
        blockify alignment = decorate (Markup.blockify alignment)

-- | Updates key assignments given a key assignment request.
updateKeys :: (Eq k) => (k, [Key]) -> Map Key k -> Map Key k
updateKeys = undefined -- TODO

-- | Performs the necessary 'Moment' operations such that, when actuated,
-- the network will allow the user to interact with an instance of the given
-- widget on the current terminal.
runWidget :: Frameworks t => Widget t Block a -> Moment t a
runWidget (Widget x) = mdo
    (gKey, onKey) <- newEvent
    -- TODO: listen for keys
    let iKeyMap = foldl (flip updateKeys) Map.empty $ initialKeys xInfo
    let keyMap = accumB iKeyMap $ updateKeys <$> keyAssign xInfo
    let aKey = filterJust $ flip Map.lookup <$> keyMap <@> gKey
    (xFig, xValue, xInfo) <- x Context {
        key = aKey }
    let (_, _, paint) = Block.place xFig width height
    size <- runPaint $ paint $ pure (Nothing, (0, 0))
    let width = fst <$> size
    let height = snd <$> size
    return xValue
