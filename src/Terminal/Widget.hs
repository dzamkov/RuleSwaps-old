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
import Terminal.Base
import Terminal.Flow (Flow, TextStyle)
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
data Context e f k = Context {

    -- | An event that occurs when an assigned key is pressed.
    keyPress :: e k,

    -- | Provides the identifier to key mapping for assigned keys.
    keyAssignments :: f (k -> Maybe Key) }

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
    => Widget (Context e f k -> Info e f k q a)

-- | Constructs a non-interactive widget with the given value and figure.
toWidget :: (Event e) => a -> d f -> Widget e f d a
toWidget value figure = Widget $ const Info {
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
            keyPress = filterJust $
                either Just (const Nothing) <$> keyPress context,
            keyAssignments = (. Left) <$> keyAssignments context }
        yInfo = y Context {
            keyPress = filterJust $
                either (const Nothing) Just <$> keyPress context,
            keyAssignments = (. Right) <$> keyAssignments context }
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
    => Markup.Flow Width (Widget e f Flow a) where
        weakSpace = toWidget mempty . Markup.weakSpace
        strongSpace = toWidget mempty . Markup.strongSpace
        tight = decorate Markup.tight
instance (Reactive e f, Monoid a)
    => Markup.FlowText Width TextStyle (Widget e f Flow a) where
        tightText style str = toWidget mempty $ Markup.tightText style str
        naturalSpace style = toWidget mempty $ Markup.naturalSpace style
instance (Reactive e f, Monoid a)
    => Markup.Block Width Height (Widget e f Block a) where
        (|||) = compose (Markup.|||)
        (===) = compose (Markup.===)
        setWidth width = decorate (Markup.setWidth width)
        setHeight height = decorate (Markup.setHeight height)
instance (Reactive e f, Monoid a)
    => Markup.BlockSolid Width Height Color (Widget e f Block a) where
        solid = toWidget mempty . Markup.solid
instance (Reactive e f, Monoid a)
    => Markup.BlockTrans Width Height (Widget e f Block a) where
        clear = toWidget mempty Markup.clear
        over = compose Markup.over
instance (Reactive e f, Monoid a) => Markup.FlowToBlock
    Width Height (Widget e f Flow a) (Widget e f Block a) where
        blockify alignment = decorate (Markup.blockify alignment)

-- | Describes the injective mapping between identifiers and keys.
data KeyMap k = KeyMap (Map k Key) (Map Key k)

-- | A 'KeyMap' with no key assignments.
emptyKeyMap :: KeyMap k
emptyKeyMap = KeyMap Map.empty Map.empty

-- | Updates a 'KeyMap' in response to a key assignment request.
updateKeyMap :: (Ord k) => (k, [Key]) -> KeyMap k -> KeyMap k
updateKeyMap (id, keys) curMap@(KeyMap f b) =
    let assign curMap@(KeyMap f b) (key : keys) =
            case Map.lookup key b of
                Just _ -> assign curMap keys
                Nothing -> KeyMap (Map.insert id key f) (Map.insert key id b)
        assign curMap [] = curMap
    in case Map.lookup id f of
        Just curKey | curKey `elem` keys -> curMap
        Just curKey ->
            let nMap = KeyMap (Map.delete id f) (Map.delete curKey b)
            in assign nMap keys
        Nothing -> assign curMap keys

-- | Gets the key for an identifier in a 'KeyMap'.
lookupKeyMap :: (Ord k) => KeyMap k -> k -> Maybe Key
lookupKeyMap (KeyMap f _) id = Map.lookup id f

-- | Gets the identifier for a key in a 'KeyMap'
reverseLookupKeyMap :: (Ord k) => KeyMap k -> Key -> Maybe k
reverseLookupKeyMap (KeyMap _ b) key = Map.lookup key b

-- | Runs a widget in the current terminal.
runWidget :: Widget IO.Event IO.Behavior Block a -> IO a
runWidget (Widget x) = mdo
    (gKey, _) <- IO.newEvent
    -- TODO: listen for keys
    let iKeyMap = foldl (flip updateKeyMap) emptyKeyMap $ initialKeys xInfo
    let keyMap = accumB iKeyMap $ updateKeyMap <$> keyAssign xInfo
    let aKey = filterJust $ reverseLookupKeyMap <$> keyMap <@> gKey
    let xInfo = x Context {
        keyPress = aKey,
        keyAssignments = lookupKeyMap <$> keyMap }
    runPaint $ \size ->
        let width = fst <$> size
            height = snd <$> size
            (_, _, paint) = Block.place (figure xInfo) width height
        in paint $ pure (Nothing, (0, 0))
    return $ value xInfo
