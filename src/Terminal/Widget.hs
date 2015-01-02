{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Terminal.Widget (
    Event (..),
    await,
    replace,
    sendParent,
    sendChild,
    Widget (..),
    Concrete (..),
    pageToWidget,
    mapFilterAll,
    mapFilterInput,
    mapFilterOutput
) where

import Terminal.Page
import Control.Monad.Operational
import Control.Monad (forever)
import Data.Void

-- | Identifies an event a widget can respond to.
data Event i o' k
    = Select k
    | ReceiveParent i
    | ReceiveChild o'

-- | An instruction that can be performed by a widget.
data WidgetInstr i o i' o' h k a where
    Await :: WidgetInstr i o i' o' h k (Event i o' k)
    Replace :: Hole h a -> Widget i' o' a -> WidgetInstr i o i' o' h k ()
    SendParent :: o -> WidgetInstr i o i' o' h k ()
    SendChild :: i' -> WidgetInstr i o i' o' h k ()

-- | Awaits an event.
await :: Program (WidgetInstr i o i' o' h k) (Event i o' k)
await = singleton Await

-- | Replaces the contents of a hole with the given widget.
replace :: Hole h a -> Widget i' o' a -> Program (WidgetInstr i o i' o' h k) ()
replace hole = singleton . Replace hole

-- | Sends a message to the parent widget.
sendParent :: o -> Program (WidgetInstr i o i' o' h k) ()
sendParent = singleton . SendParent

-- | Sends a message to the child widget.
sendChild :: i' -> Program (WidgetInstr i o i' o' h k) ()
sendChild = singleton . SendChild

-- | Contains the internal information for a widget.
data Internal i o i' o' h k a = Internal {

    -- | The immutable page for this widget.
    page :: Page h k a,

    -- | The initial child widgets to fill the holes for the widget.
    holes :: h (Widget i' o'),

    -- | The program that implements the logic for this widget. When the
    -- program finishes, the widget will be replaced by the returned widget.
    program :: Program (WidgetInstr i o i' o' h k) (Widget i o a) }

-- | A figure-like which enables stateful interactivity.
data Widget i o a = forall i' o' h k. (Ord k)
    => Widget (Internal i o i' o' h k a)

-- | Converts a concrete page into a simple widget which emits an event when
-- an option is selected.
pageToWidget :: (Ord k) => Page Concrete k a -> Widget Void k a
pageToWidget page = Widget Internal {
    page = page,
    holes = Concrete,
    program = forever $ do
        event <- await
        case event of
            Select option -> sendParent option
            _ -> undefined }

-- | Maps and filters the messages from and to a widget.
mapFilterAll :: (i -> Maybe ni) -> (o -> Maybe no)
    -> Widget ni o a -> Widget i no a
mapFilterAll f g (Widget internal) = res where
    nProgram = interpretWithMonad (\instr -> case instr of
        Await ->
            let awaitGood = do
                event <- await
                case event of
                    Select option -> return (Select option)
                    ReceiveParent input -> case f input of
                        Just nInput -> return (ReceiveParent nInput)
                        Nothing -> awaitGood
                    ReceiveChild msg -> return (ReceiveChild msg)
            in awaitGood
        Replace hole widget -> replace hole widget
        SendParent output -> case g output of
            Just nOutput -> sendParent nOutput
            Nothing -> return ()
        SendChild msg -> sendChild msg) $ program internal
    res = Widget Internal {
        page = page internal,
        holes = holes internal,
        program = fmap (mapFilterAll f g) nProgram }

-- | Maps and filters the input to a widget.
mapFilterInput :: (i -> Maybe n) -> Widget n o a -> Widget i o a
mapFilterInput f = mapFilterAll f Just

-- | Maps and filters the output from a widget.
mapFilterOutput :: (o -> Maybe n) -> Widget i o a -> Widget i n a
mapFilterOutput = mapFilterAll Just
