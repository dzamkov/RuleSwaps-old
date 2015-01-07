{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module Terminal.Page (
    Key,
    NavigaTree,
    Direction (..),
    NavigaCursor,
    navigate,
    Context (..),
    Page (..),
    figureToPage,
    highlightFlow,
    option,
    keysViewD,
    keyView,
    hole
) where

import Delta
import Terminal.Draw (FullColor)
import Terminal.Figure
import Data.Functor.Compose
import Data.Maybe (maybeToList, fromMaybe)
import Control.Monad.State (State, runState, modify)
import Control.Monad.Reader (Reader, runReader, ask)
import Control.Applicative

-- | Identifies an assignable shortcut key.
type Key = Char

-- | Describes the navigational layout of the elements in a page.
data NavigaTree h k
    = Option k (Maybe (NavigaTree h k))
    | forall a. Hole (h a)
    | Linear Axis [NavigaTree h k] Bool Bool

-- | Tries constructing a linear 'NavigaTree' with the given axis, options,
-- and whether leaving the list is possible along the beginning and end.
linear :: Axis -> [NavigaTree h k] -> Bool -> Bool -> Maybe (NavigaTree h k)
linear _ [] _ _ = Nothing
linear _ [single] True True = Just single
linear axis options l r = Just $ Linear axis options l r

-- | Identifies a direction in which navigation is possible.
data Direction = Left | Up | Right | Down

-- | A zipper for a 'NavigaTree'.
data NavigaCursor h k = NavigaCursor

-- | Advances a certain direction within a 'NavigaCursor'.
navigate :: Direction -> NavigaCursor h k -> Maybe (NavigaCursor h k)
navigate _ NavigaCursor = undefined -- TODO

-- | Contains the information needed to create a (delta for a) figure for a
-- page.
data Context (h :: * -> *) k = Context {

    -- | Gets the key assigned to the given option and whether it is selected.
    getOption :: k -> Delta (Maybe Key, Bool),

    -- | Gets the (delta for the) figure which will fill the given hole.
    getHole :: forall a. h a -> Delta (Figure a) }

-- | A figure-like which enables limited interactivity. It has several options
-- which may be highlighted and selected, and has several "holes" which can
-- be filled with other figure-likes.
data Page h k a = Page {

    -- | Contains the options in this page, associated with the possible
    -- shortcut keys that may be used, ordered by preference.
    shortcuts :: [(k, [Key])],

    -- | The navigational layout of this page.
    navigatree :: Maybe (NavigaTree h k),

    -- | Gets the figure for this page using the given context.
    figure :: Context h k -> Delta (Figure a) }

{-# ANN module "HLint: ignore Use ***" #-}
instance Ord k => FigureLike (Page h k) where
    compose hint inner = toPage (inner evalInner) where
        evalInner page = Compose $
            modify (\(s, t) ->
                (shortcuts page ++ s,
                maybeToList (navigatree page) ++ t)) *>
            pure (figure page <$> ask)
        toPage :: Compose
            (State ([(k, [Key])], [NavigaTree h k]))
            (Reader (Context h k))
            (Delta (Figure a)) -> Page h k a
        toPage (Compose state) = res where
            (reader, (s, t)) = runState state ([], [])
            axis = fromMaybe Horizontal hint
            res = Page {
                shortcuts = s,
                navigatree = linear axis t True True,
                figure = runReader reader }

-- | Converts a figure into a static page.
figureToPage :: (Ord k) => Figure a -> Page h k a
figureToPage = generalize

-- | A possible highlight function for 'option' which sets the back color of
-- a flow when selected.
highlightFlow :: FullColor -> Delta Bool
    -> Delta (Figure Flow) -> Delta (Figure Flow)
highlightFlow back selected source = figureD (layoutD source) $
    funD (\(area, normal, offset) ->
            let back' True = back
                back' False = normal
                placement = (\s -> (area, back' s, offset)) <$> selected
            in placeD source <*> placement)

-- | Converts a page into an option, at the top level, by specifying an
-- identifier, possible shortcut keys (in order of preference), and a function
-- which highlights the figure.
option :: k -> [Key]
    -> (Delta Bool -> Delta (Figure a) -> Delta (Figure a))
    -> Page h k a -> Page h k a
option id keys decorate page = Page {
    shortcuts = (id, keys) : shortcuts page,
    navigatree = Just $ Option id $ navigatree page,
    figure = \context -> decorate
        (sndD $ getOption context id)
        (figure page context) }

-- | Creates a page which displays information about key assignments.
keysViewD :: ((k -> Delta (Maybe Key)) -> Delta (Figure a)) -> Page h k a
keysViewD figure = Page {
    shortcuts = [],
    navigatree = Nothing,
    figure = \context -> figure (fstD . getOption context) }

-- | Creates a page which displays information about a single key assignment.
keyView :: k -> (Maybe Key -> Figure a) -> Page h k a
keyView id figure = keysViewD (\getKey -> pure figure <*> getKey id)

-- | Constructs a page for the given hole.
hole :: h a -> Page h k a
hole hole = Page {
    shortcuts = [],
    navigatree = Just $ Hole hole,
    figure = (`getHole` hole) }
