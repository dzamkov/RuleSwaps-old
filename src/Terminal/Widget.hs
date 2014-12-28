{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Terminal.Widget (
    Widget (..),
    toWidget,
    runWidget
) where

import Stride
import Stride.Map
import Signal
import Terminal.Draw
import Terminal.Figure hiding (end)
import Terminal.Input
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Functor.Compose
import Control.Monad (replicateM_)
import Control.Monad.Reader (Reader, runReader, ask)
import Control.Monad.State (State, runState, modify)
import Control.Monad.Trans (lift)
import Control.Applicative hiding (empty)
import System.Console.ANSI
import qualified System.Console.Terminal.Size as Size

-- | An assignable key that the user can press.
type Key = Char

-- | A figure which exposes some pressable keys, allowing interactivity.
-- @a@ is the layout type while @k@ is an identifier for information/action
-- associated with a key.
data Interact k a = Interact {

    -- | The set of keys the 'Interact' offers to the user, each associated
    -- with the prefered key to be assigned to.
    keys :: Map k Key,

    -- | Given a mapping from key identifiers to keys, gets the figure used to
    -- display this 'Interact'.
    assign :: (k -> Key) -> Figure a }

type instance Complex (Interact k a) = CInteract k a
data CInteract k a = CInteract (Stride (Map k Key))
    (Stride ((k -> Key) -> Figure a))
instance IsStride (CInteract k a) where
    glue (CInteract ak af) (CInteract bk bf) =
        CInteract (glue ak bk) (glue af bf)
instance StrideRel (CInteract k a) (Interact k a) where
    start (CInteract k f) = Interact {
        keys = start k,
        assign = start f }
    end (CInteract k f) = Interact {
        keys = end k,
        assign = end f }

-- | Extracts 'keys' from the stride of an 'Interact'.
keysS :: Stride (Interact k a) -> Stride (Map k Key)
keysS (Complex (CInteract k _)) = k
keysS s = keys <$> s

-- | Extracts 'assign' from the stride of an 'Interact'.
assignS :: Stride (Interact k a) -> Stride ((k -> Key) -> Figure a)
assignS (Complex (CInteract _ a)) = a
assignS s = assign <$> s

-- | A dynamic interactive figure of layout type @a@ within the context of a
-- 'ReactT' @r m@.
newtype Widget r k a = Widget (Signal r (Interact k a))

instance Ord k => FigureLike (Widget r k) where
    compose inner = res where
        res = Widget $ deltor (\eval ->
            toInteract <$> (getCompose $ inner (innerEval eval)))
        innerEval eval (Widget w) = Compose (interact' <$> eval w) where
            interact' i = Compose $
                modify (\curKeys -> dunion curKeys $ keysS i) *>
                pure ((\a -> assignS i <*> pure a) <$> ask)
        toInteract :: Compose
            (State (Stride (Map k Key)))
            (Reader (k -> Key))
            (Stride (Figure a))
            -> Stride (Interact k a)
        toInteract (Compose source) = res where
            (assign', keys) = runState source $ stay $ Map.empty
            assign = Complex $ \m -> runReader assign' m
            res = Complex $ CInteract keys assign

-- | Converts a figure into a widget.
toWidget :: (Ord k) => Figure a -> Widget r k a
toWidget = generalize

-- | Runs a widget signal within the context of a 'ReactT' @r@ 'IO'.
runWidget :: (Ord k) => (k -> ReactT r IO ())
    -> Widget r k Dock -> ReactT r IO ()
runWidget action (Widget widget) = do
    (curInteract, getInteract) <- output widget
    size <- lift (fromJust <$> Size.size)
    let width = Size.width size :: Int
    let height = Size.height size :: Int
    let rest = (width, (0, 0), defaultAppearance)
    st <- lift $ do
        hideCursor
        setAppearance defaultAppearance
        replicateM_ height $ putStrLn ""
        cursorUp height
        return rest
    let fig = assign curInteract undefined
    let figP = place fig (width, height)
    let figD = draw $ place figP (0, 0)
    st <- lift $ do
        st <- runDraw figD st
        changeState st rest
        return rest
    lift getHiddenChar
    return ()
