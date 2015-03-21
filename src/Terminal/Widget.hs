{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Terminal.Widget (
    Widget,
    ButtonStyle (..),
    runWidget
) where

import Reactive
import qualified Reactive.IO as IO
import qualified Markup
import Markup.Attr hiding (key)
import Terminal.Base
import Terminal.Flow (Flow, TextStyle)
import Terminal.Block (Block)
import Terminal.Paint (runPaint)
import qualified Terminal.Block as Block
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Monoid
import Control.Monad.State
import Control.Arrow (first, second)
import Control.Applicative

-- | Identifies a key that can be assigned to an action.
type Key = Char

-- | @m@ is a monadic procedure that sets or changes a reactive network
-- (with events of type @e@ and behaviors of type @f@) from the perspective
-- of a widget.
class (Reactive e f, Applicative m, MonadFix m)
    => MonadWidget e f m | m -> e f where

    -- | Tries capturing the given key. If successful, the event for when the
    -- is pressed is returned, along with a procedure that can be used to
    -- later release the key.
    requestKey :: Key -> m (Maybe (e (), m ()))

    -- | Allows a procedure to be executed at a later time. Whenever the given
    -- event occurs, the associated procedure will be run and its result will
    -- be accessible from the returned event.
    defer :: e (m a) -> m (e a)

-- | Tries capturing one of the given keys, ordered by preference. See
-- 'requestKey'.
requestKeyFrom :: (MonadWidget e f m) => [Key] -> m (Maybe (Key, e (), m ()))
requestKeyFrom [] = return Nothing
requestKeyFrom (h : t) = do
    r <- requestKey h
    case r of
        Just (event, release) -> return $ Just (h, event, release)
        Nothing -> requestKeyFrom t

-- | Extends a figure of type @q@ with user interaction capabilities, such as
-- selection and key input. An active instance of the widget produces a value
-- of type @a@. Widget's live in a reactive system with events of type @e@
-- and behaviors of type @f@.
data Widget e f q a = Widget (forall m. (MonadWidget e f m) => m (q f, a))
instance Functor (Widget e f q) where
    fmap f (Widget x) = Widget (fmap (second f) x)
instance (Reactive e f) => Markup.Widget (Widget e f q) where
    wfix f = Widget $ mdo
        let Widget x = f value
        (fig, value) <- x
        return (fig, value)
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

-- | Constructs a non-interactive widget with the given value and figure.
toWidget :: (Event e) => a -> d f -> Widget e f d a
toWidget value figure = Widget $ return (figure, value)

-- | Decorates a widget using the given function to decorate the underlying
-- figure.
decorate :: (q f -> p f) -> Widget e f q a -> Widget e f p a
decorate f (Widget source) = Widget $ first f <$> source

-- | Composes two widgets using the given function to compose the underlying
-- figures.
compose :: (Reactive e f, Monoid a)
    => (q f -> p f -> r f)
    -> Widget e f q a -> Widget e f p a -> Widget e f r a
compose f (Widget x) (Widget y) = Widget $
    (\(xFig, xV) (yFig, yV) -> (f xFig yFig, xV <> yV)) <$> x <*> y

-- | A possible style for a flow-based button.
data ButtonStyle = ButtonStyle {
    buttonKeys :: [Key],
    buttonColor :: Color,
    buttonTitle :: Maybe String }
instance AttrKey ButtonStyle where
    keys k style = style { buttonKeys = k }
instance AttrColor Color ButtonStyle where
    color c style = style { buttonColor = c }
instance AttrTitle ButtonStyle where
    title t style = style { buttonTitle = Just t }
instance HasDefault ButtonStyle where
    defaultStyle = ButtonStyle {
        buttonKeys = [],
        buttonColor = snd defaultAppearance,
        buttonTitle = Nothing }
instance Reactive e f
    => Markup.WidgetButton e ButtonStyle (Widget e f Flow) where
        button mod = res where
            style = mod defaultStyle
            indicator key = Markup.tightText
                (color $ buttonColor style)
                ['[', key, ']']
            title = Markup.tightText
                (color $ buttonColor style)
                <$> buttonTitle style
            res = Widget $ do
                r <- requestKeyFrom $ buttonKeys style
                let (ind, event) = case r of
                        Just (key, event, _) -> (Just $ indicator key, event)
                        Nothing -> (Nothing, never)
                let fig = case (ind, title) of
                        (Nothing, Nothing) -> mempty
                        (Just ind, Nothing) -> ind
                        (Nothing, Just title) -> title
                        (Just ind, Just title) ->
                            ind <> Markup.space 1 <> title
                return (fig, event)

-- | Contains information about a running widget from a terminal perspective.
data GlobalState = GlobalState {

    -- | An event that is fired when an assignable key is pressed.
    key :: IO.Event Key,

    -- | The set of keys that are currently assigned.
    assignedKeys :: Set Key }

-- | Implementation of 'MonadWidget' from a terminal perspective.
newtype Global a = Global (StateT GlobalState IO a)
    deriving (Functor, Applicative, Monad, MonadFix)
instance MonadWidget IO.Event IO.Behavior Global where
    requestKey rKey = Global $ do
        state <- get
        let curKeys = assignedKeys state
        if Set.member rKey curKeys
            then return Nothing
            else do
                put state { assignedKeys = Set.insert rKey curKeys }
                let event = filterJust $ (\eKey -> if eKey == rKey
                        then Just () else Nothing) <$> key state
                    release = Global $ modify (\state -> state {
                        assignedKeys = Set.delete rKey $ assignedKeys state })
                return $ Just (event, release)
    defer = undefined -- TODO

-- | Runs a widget in the current terminal.
runWidget :: Widget IO.Event IO.Behavior Block a -> IO a
runWidget (Widget (Global setup)) = do
    (key, _) <- IO.newEvent
    let state = GlobalState {
            key = key,
            assignedKeys = Set.empty }
    (fig, value) <- evalStateT setup state
    runPaint $ \size ->
        let width = fst <$> size
            height = snd <$> size
            (_, _, paint) = Block.place fig width height
        in paint $ pure (Nothing, (0, 0))
    return value
