{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
module UI (
    Input (..),
    Output (..),
    Style (main),
    Casino (..)
) where

import Markup hiding (Widget, Block)
import Markup.Attr
import Markup.Builder
import Reactive
import Terminal.Flow (Flow)
import Terminal.Block (Block)
import Terminal.Base
import Terminal.Widget
import qualified System.Console.ANSI as ANSI

-- | The input to the interface.
data Input (e :: * -> *) (f :: * -> *) = Input { }

-- | The output from the interface.
data Output e (f :: * -> *) = Output {

    -- | an event occurs when the user wants to quit the program.
    quit :: e () }

-- | @a@ provides styling information for a user interface.
class Style a where

    -- | The default back color for the screen.
    back :: (?style :: a) => Color

    -- | The main interface for the program.
    main :: (Reactive e f, ?style :: a)
        => Input e f -> Widget e f Block (Output e f)
    main input = runBuilder $ do
        (quit, quitE) <- use $ button (key 'q' . title "Quit")
        return (setBack back $ blockify Center quit, Output { quit = quitE })

-- | A style which imitates a casino poker table.
data Casino = Casino
instance Style Casino where
    back = Color ANSI.Dull ANSI.Green
