{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecursiveDo #-}
module Terminal.Widget (
    Widget,
    runWidget
) where

import Reactive
import qualified Reactive.IO as IO
import qualified Markup
import Terminal.Context (Terminal)
import Terminal.Flow (Flow)
import Terminal.Block (Block)
import Terminal.Paint (runPaint)
import qualified Terminal.Block as Block
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Control.Arrow (first)
import Control.Applicative

-- | Identifies a key that can be assigned to an action.
type Key = Char

-- | The information needed to instantiate a widget.
data Context e k = Context {

    -- | An event that occurs when an assigned key is pressed.
    key :: e k }

-- | The information provided by a running widget.
data Info e (f :: * -> *) k q a = Info {

    -- | The initial requested keys for the widget. Each identifier is
    -- associated with a list of keys that may be assigned to it, in order
    -- of preference.
    initialKeys :: [(k, [Key])],

    -- | An event that occurs when a key request for the widget changes. Each
    -- event consists of an identifier an a list of keys that may be assigned
    -- to it, in order of preference. This may cause an existing identifier to
    -- be reassigned or cleared (if the key list is empty).
    keyAssign :: e (k, [Key]),

    -- | The figure for the widget.
    figure :: q f,

    -- | The value of the widget.
    value :: a }

-- | Extends a figure of type @q@ with user interaction capabilities, such as
-- selection and key input. An active instance of the widget produces a value
-- of type @a@. Widget's live in a reactive system with events of type @e@
-- and behaviors of type @f@.
data Widget e f q a = forall k. (Ord k)
    => Widget (Context e k -> Info e f k q a)

-- | Constructs a non-interactive widget with the given value and figure.
toWidget :: (Event e) => a -> d f -> Widget e f d a
toWidget value figure = Widget $ const $ Info {
    initialKeys = [] :: [((), [Key])],
    keyAssign = never,
    figure = figure,
    value = value }

-- | Decorates a widget using the given function to decorate the underlying
-- figure.
decorate :: (q f -> p f) -> Widget e f q a -> Widget e f p a
decorate decorateFigure (Widget x) = Widget $ \context ->
    let xInfo = x context
    in xInfo { figure = decorateFigure $ figure xInfo }

-- | Composes two widgets using the given function to compose the underlying
-- figures.
compose :: (Reactive e f, Monoid a)
    => (q f -> p f -> r f)
    -> Widget e f q a -> Widget e f p a -> Widget e f r a
compose composeFigures (Widget x) (Widget y) = Widget $ \context ->
    let xInfo = x Context {
            key = filterJust $ either Just (const Nothing) <$> key context }
        yInfo = y Context {
            key = filterJust $ either (const Nothing) Just <$> key context }
    in Info {
        initialKeys = fmap (first Left) (initialKeys xInfo) ++
            fmap (first Right) (initialKeys yInfo),
        keyAssign = fmap (first Left) (keyAssign xInfo) `union`
            fmap (first Right) (keyAssign yInfo),
        figure = composeFigures (figure xInfo) (figure yInfo),
        value = value xInfo <> value yInfo }

instance (Reactive e f, Monoid a) => Monoid (Widget e f Flow a) where
    mempty = toWidget mempty mempty
    mappend = compose mappend
instance (Reactive e f, Monoid a)
    => Markup.Flow Terminal (Widget e f Flow a) where
        weakSpace = toWidget mempty . Markup.weakSpace
        strongSpace = toWidget mempty . Markup.strongSpace
        tightText font fore str = toWidget mempty $
            Markup.tightText font fore str
instance (Reactive e f, Monoid a)
    => Markup.Block Terminal (Widget e f Block a) where
        clear = toWidget mempty Markup.clear
        (|||) = compose (Markup.|||)
        (===) = compose (Markup.===)
        over = compose Markup.over
        setWidth width = decorate (Markup.setWidth width)
        setHeight height = decorate (Markup.setHeight height)
        setBack nBack = decorate (Markup.setBack nBack)
instance (Reactive e f, Monoid a) => Markup.FlowToBlock Terminal
    (Widget e f Flow a) (Widget e f Block a) where
        blockify alignment = decorate (Markup.blockify alignment)

-- | Updates key assignments given a key assignment request.
updateKeys :: (Eq k) => (k, [Key]) -> Map Key k -> Map Key k
updateKeys = undefined -- TODO

-- | Runs a widget in the current terminal.
runWidget :: Widget IO.Event IO.Behavior Block a -> IO a
runWidget (Widget x) = mdo
    (gKey, _) <- IO.newEvent
    -- TODO: listen for keys
    let iKeyMap = foldl (flip updateKeys) Map.empty $ initialKeys xInfo
    let keyMap = accumB iKeyMap $ updateKeys <$> keyAssign xInfo
    let aKey = filterJust $ flip Map.lookup <$> keyMap <@> gKey
    let xInfo = x Context { key = aKey }
    runPaint $ \size ->
        let width = fst <$> size
            height = snd <$> size
            (_, _, paint) = Block.place (figure xInfo) width height
        in paint $ pure (Nothing, (0, 0))
    return $ value xInfo
