{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Terminal.Page (
    Key,
    Navigatree (..),
    Hole,
    defHole,
    getHole,
    modifyHole,
    setHole,
    AnyHole (..),
    Context (..),
    Page (..),
    figureToPage,
    highlightFlow,
    option,
    keyView,
    hole
) where

import Stride
import Terminal.Draw (FullColor)
import Terminal.Figure
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor.Compose
import Data.Maybe (maybeToList, fromMaybe)
import Control.Monad.Identity (Identity (..))
import Control.Monad.State (State, runState, modify)
import Control.Monad.Reader (Reader, runReader, ask)
import Control.Applicative

-- | Identifies an assignable shortcut key.
type Key = Char

-- | Describes the navigational layout of the elements in a page.
data Navigatree k h
    = Option k (Maybe (Navigatree k h))
    | forall a. Hole (Hole h a)
    | Linear Axis [Navigatree k h] Bool Bool

-- | Tries constructing a linear 'Navigatree' with the given axis, options,
-- and whether leaving the list is possible along the beginning and end.
linear :: Axis -> [Navigatree k h] -> Bool -> Bool -> Maybe (Navigatree k h)
linear _ [] _ _ = Nothing
linear _ [single] True True = Just single
linear axis options l r = Just $ Linear axis options l r

-- | Identifies a hole within a product type containing several holes. Note
-- the similarity to a lens.
type Hole h a = forall f g. (Functor f) => (g a -> f (g a)) -> h g -> f (h g)

-- | Constructs a hole given a 'get' function and a 'set' function.
defHole :: (forall f. h f -> f a) -> (forall f. f a -> h f -> h f) -> Hole h a
defHole get set return struct = fmap (`set` struct) $ return $ get struct

-- | Gets the value of a hole in a hole-carrying structure.
getHole :: Hole h a -> h f -> f a
getHole hole struct = getConst $ hole Const struct

-- | Modifies the value of a hole in a hole-carrying structure.
modifyHole :: Hole h a -> (f a -> f a) -> h f -> h f
modifyHole hole f struct = runIdentity $ hole (Identity . f) struct

-- | Sets the value of a hole in a hole-carrying structure.
setHole :: Hole h a -> f a -> h f -> h f
setHole hole value = modifyHole hole (const value)

-- | Identifies a hole of any layout type.
data AnyHole h = forall a. AnyHole (Hole h a)

-- | Contains the information needed to create a (stride of a) figure for a
-- page.
data Context k h = Context {

    -- | The keys assigned to each option in the page.
    keys :: k -> Maybe Key,

    -- | Gets the (stride for the) figure which will fill the given hole.
    fill :: forall a. Hole h a -> Stride (Figure a),

    -- | Gets the currently selected option.
    selected :: Stride (Maybe k) }

-- | A figure-like which enables limited interactivity. It has several options
-- which may be highlighted and selected, and has several "holes" which can
-- be filled with other figure-likes.
data Page k h a = Page {

    -- | Contains the options in this page, associated with the possible
    -- shortcut keys that may be used, ordered by preference.
    shortcuts :: Map k [Key],

    -- | The navigational layout of this page.
    navigatree :: Maybe (Navigatree k h),

    -- | The set of holes this page contains.
    holes :: [AnyHole h],

    -- | Gets the figure for this page using the given context.
    figure :: Context k h -> Stride (Figure a) }

instance Ord k => FigureLike (Page k h) where
    compose hint inner = toPage (inner evalInner) where
        evalInner page = Compose $
            modify (\(s, t, h) ->
                (Map.union s (shortcuts page),
                maybeToList (navigatree page) ++ t,
                holes page ++ h)) *>
            pure (figure page <$> ask)
        toPage :: Compose
            (State (Map k [Key], [Navigatree k h], [AnyHole h]))
            (Reader (Context k h))
            (Stride (Figure a)) -> Page k h a
        toPage (Compose state) = res where
            (reader, (s, t, h)) = runState state (Map.empty, [], [])
            axis = fromMaybe Horizontal hint
            res = Page {
                shortcuts = s,
                navigatree = linear axis t True True,
                holes = h,
                figure = runReader reader }

-- | Converts a figure into a static page.
figureToPage :: (Ord k) => Figure a -> Page k h a
figureToPage = generalize

-- | A possible highlight function for 'option' which sets the back color of
-- a flow when selected.
highlightFlow :: FullColor -> Stride Bool
    -> Stride (Figure Flow) -> Stride (Figure Flow)
highlightFlow back selected source = figureS (layoutS source) $
    funS (\(area, normal, offset) ->
            let back' True = back
                back' False = normal
                placement = (\s -> (area, back' s, offset)) <$> selected
            in placeS source <*> placement)

-- | Converts a page into an option, at the top level, by specifying an
-- identifier, possible shortcut keys (in order of preference), and a function
-- which highlights the figure.
option :: (Ord k) => k -> [Key]
    -> (Stride Bool -> Stride (Figure a) -> Stride (Figure a))
    -> Page k h a -> Page k h a
option id keys decorate page = Page {
    shortcuts = Map.insert id keys $ shortcuts page,
    navigatree = Just $ Option id $ navigatree page,
    holes = holes page,
    figure = \context -> decorate
        (checkS $ (== Just id) <$> selected context)
        (figure page context) }

-- | Creates a page which displays information about key assignments.
keyView :: ((k -> Maybe Key) -> Figure a) -> Page k h a
keyView figure = Page {
    shortcuts = Map.empty,
    navigatree = Nothing,
    holes = [],
    figure = stay . figure . keys }

-- | Constructs a page for the given hole.
hole :: Hole h a -> Page k h a
hole hole = Page {
    shortcuts = Map.empty,
    navigatree = Just $ Hole hole,
    holes = [AnyHole hole],
    figure = (`fill` hole) }
