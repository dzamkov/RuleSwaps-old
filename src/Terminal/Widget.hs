{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Terminal.Widget (
    Widget (..),
    runWidget,
    figure
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
import Control.Monad (replicateM_)
import Control.Monad.Trans (lift)
import Control.Applicative hiding (empty)
import System.Console.ANSI
import qualified System.Console.Terminal.Size as Size

-- | An assignable key that the user can press.
type Key = Char

-- | A figure which exposes some pressable keys, allowing interactivity.
-- @a@ is the layout type while @k@ is an identifier for information/action
-- associated with a key.
data Widget k a = Widget {

    -- | The set of keys the widget offers to the user, each associated with
    -- the prefered key to be assigned to.
    keys :: Map k Key,

    -- | Given a mapping from key identifiers to keys, gets the figure used to
    -- display this widget.
    assign :: (k -> Key) -> Figure a }

data CWidget k a = CWidget (Stride (Map k Key))
    ((k -> Key) -> Stride (Figure a))
instance IsStride (CWidget k a) where
    glue (CWidget ak af) (CWidget bk bf) = CWidget (glue ak bk) (glue af bf)
instance StrideRel (CWidget k a) (Widget k a) where
    start (CWidget k f) = Widget {
        keys = start k,
        assign = start f }
    end (CWidget k f) = Widget {
        keys = end k,
        assign = end f }

-- | Runs a widget signal within the context of a 'ReactT' of an 'IO'.
runWidget :: (Ord k) => (k -> ReactT r IO ())
    -> Signal r (Widget k Dock) -> ReactT r IO ()
runWidget action widget = do
    (curWidget, getWidget) <- output widget
    size <- lift (fromJust <$> Size.size)
    let width = Size.width size :: Int
    let height = Size.height size :: Int
    let rest = (width, (0, 0), defaultAppearance)
    st <- lift $ do
        setAppearance defaultAppearance
        replicateM_ height $ putStrLn ""
        cursorUp height
        return rest
    let fig = assign curWidget undefined
    let figP = place fig (width, height)
    let figD = draw $ place figP (0, 0)
    st <- lift $ do
        st <- runDraw figD st
        changeState st rest
        return rest
    lift getHiddenChar
    return ()

-- | Converts a figure into a widget.
figure :: Figure a -> Widget k a
figure fig = Widget {
    keys = Map.empty,
    assign = const fig }
