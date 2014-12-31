module Terminal.Widget (

) where

import Terminal.Input
import Terminal.Draw
import Terminal.Figure
import System.Console.ANSI
import qualified System.Console.Terminal.Size as Size
import Control.Applicative

-- | Identifies an assignable key on the keyboard.
type Key = Char

-- | Gives context information for a widget.
data Context t = Context {
    selected :: Behavior t (Maybe k),
    assignedKeys :: Behavior t (k -> Maybe Key)
    }

-- | Gives state information for a widget.
data State t a = State {
    keys :: Behavior t ([k, [Key]]),
    display :: Behavior t (Figure a) }

-- | Describes an interactive, dynamic figure of layout type @a@. @k@ is an
-- identifier for selectable sub-widgets.
data Widget t k a =
