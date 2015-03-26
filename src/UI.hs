{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
module UI (
    Input (..),
    Output (..),
    Style (main),
    Casino (..)
) where

import qualified Markup
import Markup hiding (Widget, Block)
import Markup.Attr
import Markup.Builder
import Reactive
import Terminal.Flow (Flow)
import Terminal.Block (Block)
import Terminal.Base
import Terminal.Widget
import qualified System.Console.ANSI as ANSI
import Data.Monoid
import Control.Applicative

-- | The input to the interface.
data Input (e :: * -> *) (f :: * -> *) = Input { }

-- | The output from the interface.
data Output e (f :: * -> *) = Output {

    -- | an event occurs when the user wants to quit the program.
    quit :: e () }

-- | Identifies an option in the main title menu.
data TitleOption = Join | Host | Quit

-- | @a@ is a block with that can be used to make a program UI.
type FullBlock pb a = (
    BlockSize Width Height a,
    BlockSolid Color a,
    BlockTrans a,
    BlockBorder pb a,
    AttrColor Color pb,
    AttrMargin Width Height pb)

-- | @a@ provides styling information for a user interface.
class Style a where

    -- | The default back color for the screen.
    screenBack :: (?style :: a) => Color

    -- | Wraps content in a page.
    page :: (?style :: a, FullBlock p b) => b -> b

    -- | The style modifier used for header text.
    headerStyle :: (?style :: a, AttrColor Color p) => p -> p
    headerStyle = id

    -- | The style modifier used for buttons.
    buttonStyle :: (?style :: a, AttrColor Color p) => p -> p
    buttonStyle = id

    -- | The title screen for the program.
    titleScreen :: (Reactive e f, ?style :: a)
        => Widget e f Block (e TitleOption)
    titleScreen = runBuilder $ do
        let mkButton k t = use $ button (key k . title t . buttonStyle)
        (join, joinE) <- mkButton 'j' "Join"
        (host, hostE) <- mkButton 'h' "Host"
        (quit, quitE) <- mkButton 'q' "Quit"
        let outE = foldl1 union [Join <$ joinE, Host <$ hostE, Quit <$ quitE]
            header = tightText headerStyle "RuleSwaps"
            bcenter = blockify Center
            contents =
                bcenter header ===
                setHeight 2 clear ===
                bcenter join ===
                bcenter host ===
                bcenter quit
            menu = page contents
        return (setBack screenBack $ inset menu, outE)

    -- | The main interface for the program.
    main :: (Reactive e f, ?style :: a)
        => Input e f -> Widget e f Block (Output e f)
    main input = runBuilder $ do
        (r, rE) <- use titleScreen
        return (r, Output { quit = const () <$> rE })

-- | A style which imitates a casino poker table.
data Casino = Casino
instance Style Casino where
    screenBack = Color ANSI.Dull ANSI.Green
    page = withBorder (color (Color ANSI.Dull ANSI.White)) .
        setBack (Color ANSI.Vivid ANSI.White) . pad 1 1 1 1
    headerStyle = color (Color ANSI.Vivid ANSI.Red)
    buttonStyle = color (Color ANSI.Vivid ANSI.Blue)
