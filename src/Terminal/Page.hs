{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module Terminal.Page (
    Key,
    NavigaTree,
    Direction (..),
    NavigaCursor,
    navigate,
    Hole,
    Holes (..),
    defHole,
    getHole,
    modifyHole,
    setHole,
    Concrete (..),
    WithHole (..),
    Context (..),
    Page (..),
    figureToPage,
    highlightFlow,
    option,
    keyView,
    hole
) where

import Delta
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
data NavigaTree h k
    = Option k (Maybe (NavigaTree h k))
    | forall a. Hole (Hole h a)
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

-- | Identifies a hole within a hole-carrying structure. Note the similarity to
-- a lens.
type Hole h a = forall f g. (Functor f) => (g a -> f (g a)) -> h g -> f (h g)

-- | @h@ is a hole-carrying structure, a product type containing several holes.
class Holes h where

    -- | Traverse a hole-carrying structure.
    traverseH :: Applicative m => (forall a. f a -> m (g a)) -> h f -> m (h g)

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

-- | A hole-carrying structure with no holes.
data Concrete (f :: * -> *) = Concrete
instance Holes Concrete where
    traverseH _ Concrete = pure Concrete

-- | Adds a hole to a hole-carrying structure.
data WithHole h a f = WithHole (f a) (h f)
-- TODO: Easy way of making hole-carrying structures.

-- | Contains the information needed to create a (delta for a) figure for a
-- page.
data Context h k = Context {

    -- | The keys assigned to each option in the page.
    keys :: k -> Maybe Key,

    -- | Gets the (delta for the) figure which will fill the given hole.
    fill :: forall a. Hole h a -> Delta (Figure a),

    -- | Gets the currently selected option.
    selected :: Delta (Maybe k) }

-- | A figure-like which enables limited interactivity. It has several options
-- which may be highlighted and selected, and has several "holes" which can
-- be filled with other figure-likes.
data Page h k a = Page {

    -- | Contains the options in this page, associated with the possible
    -- shortcut keys that may be used, ordered by preference.
    shortcuts :: Map k [Key],

    -- | The navigational layout of this page.
    navigatree :: Maybe (NavigaTree h k),

    -- | Gets the figure for this page using the given context.
    figure :: Context h k -> Delta (Figure a) }

instance Ord k => FigureLike (Page h k) where
    compose hint inner = toPage (inner evalInner) where
        evalInner page = Compose $
            modify (\(s, t) ->
                (Map.union s (shortcuts page),
                maybeToList (navigatree page) ++ t)) *>
            pure (figure page <$> ask)
        toPage :: Compose
            (State (Map k [Key], [NavigaTree h k]))
            (Reader (Context h k))
            (Delta (Figure a)) -> Page h k a
        toPage (Compose state) = res where
            (reader, (s, t)) = runState state (Map.empty, [])
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
option :: (Ord k) => k -> [Key]
    -> (Delta Bool -> Delta (Figure a) -> Delta (Figure a))
    -> Page h k a -> Page h k a
option id keys decorate page = Page {
    shortcuts = Map.insert id keys $ shortcuts page,
    navigatree = Just $ Option id $ navigatree page,
    figure = \context -> decorate
        (checkD $ (== Just id) <$> selected context)
        (figure page context) }

-- | Creates a page which displays information about key assignments.
keyView :: ((k -> Maybe Key) -> Figure a) -> Page h k a
keyView figure = Page {
    shortcuts = Map.empty,
    navigatree = Nothing,
    figure = keep . figure . keys }

-- | Constructs a page for the given hole.
hole :: Hole h a -> Page h k a
hole hole = Page {
    shortcuts = Map.empty,
    navigatree = Just $ Hole hole,
    figure = (`fill` hole) }
