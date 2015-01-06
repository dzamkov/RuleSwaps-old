{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
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
    mapFilterOutput,
    runWidget
) where

import Delta
import Terminal.Input
import Terminal.Draw hiding (fill)
import Terminal.Figure
import Terminal.Page hiding (Left, Right, figure, navigate)
import qualified Terminal.Page as Page
import Data.Functor.Compose
import Data.Void
import Data.Traversable
import Data.IORef
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map as Map
import qualified System.Console.Terminal.Size as Size
import Control.Monad.Operational
import Control.Monad.State (runState, get, put)
import Control.Monad (forever, replicateM_)
import Control.Applicative

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
data Widget i o a = forall i' o' h k. (Holes h, Ord k)
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


-- | The procedures needed to run a widget that is shared by all running
-- widgets.
data GlobalContext = GlobalContext {

    -- | Request keys. Each request consists of a procedure, to be called
    -- when the key is pressed, and a list of possible keys, ordered by
    -- preference. The actual key assignments are returned.
    allocKeys :: forall t. (Traversable t)
        => t (IO (), [Key]) -> IO (t (Maybe Key)),

    -- | Free assigned keys, so that they may be assigned to other widgets.
    freeKeys :: [Key] -> IO (),

    -- | Requests keyboard focus. The procedure to process key input is
    -- given. If successful, the procedure to release focus is returned.
    focus :: (Char -> IO ()) -> IO (Maybe (IO ())) }

-- | The procedures needed to run a widget.
data InstanceContext o = InstanceContext {

    -- | Sends an output message.
    send :: o -> IO (),

    -- | Requests redraw. This will be called when the widget is
    -- instantiated.
    redraw :: IO () }

-- | The control procedures provided by a running widget.
data InstanceControl i a = InstanceControl {

    -- | Receives an input message.
    receive :: i -> IO (),

    -- Notifies of a change of selection into or within the widget. Returns
    -- whether the selection stays within the widget.
    navigate :: Direction -> IO Bool,

    -- Gets the delta for the figure used to display this widget since the
    -- last call to this function. The parameter indicates whether any
    -- widget has focus.
    figure :: IO (Delta (Bool -> Figure a)),

    -- Notifies the widget that it is being destroyed/replaced. This causes
    -- the widget to free its owned keys, and possibly keyboard focus.
    destroy :: IO () }

-- | The internal implementation of 'runWidget'.
runWidget' :: forall i o a. GlobalContext
    -> InstanceContext o
    -> IORef (InstanceControl i a)
    -> Widget i o a -> IO ()
runWidget' global context controlRef = res where
    res (Widget internal) = runInternal internal
    runInternal :: forall i' o' h k. (Holes h, Ord k)
        => Internal i o i' o' h k a -> IO ()
    runInternal internal = mdo

        -- Shortcuts
        keys <- allocKeys global $
            Map.mapWithKey (\id keys -> (onKey id, keys)) $
            shortcuts $ page internal
        let onKey = process . Select -- TODO: highlight instead of select
        let destroyThis = freeKeys global $ catMaybes $ Map.elems keys

        -- Drawing
        figureRef <- newIORef (Left True)
        let figureThis = do
            figureCache <- readIORef figureRef
            let childFigure childControlRef = do
                childControl <- readIORef $ getCompose childControlRef
                childFigure <- figure childControl
                return $ Compose $ Compose childFigure
            case figureCache of
                (Right cache) -> return $ keep cache
                (Left firstTime) -> do
                    childFigures <- traverseH childFigure children
                    let result' = funD (\anyFocus ->
                          Page.figure (page internal) Page.Context {
                            keys = if anyFocus
                                then const Nothing
                                else (Map.!) keys,
                            fill = \hole -> getCompose (getCompose $
                                getHole hole childFigures) <*> pure anyFocus,
                            selected = keep Nothing {- TODO -} })
                    let result = if firstTime
                          then set (final result')
                          else result'
                    writeIORef figureRef $ Right $ final result
                    return result
        let redrawThis = do
            figureCache <- readIORef figureRef
            case figureCache of
                Right _ -> do
                    writeIORef figureRef $ Left False
                    redraw context
                _ -> return ()

        -- Children
        let childContext = InstanceContext {
            send = process . ReceiveChild,
            redraw = redrawThis }
        let setupChild :: forall b. Widget i' o' b
                -> IO (Compose IORef (InstanceControl i') b)
            setupChild child = do
                childControlRef <- newIORef undefined
                runWidget' global childContext childControlRef child
                return $ Compose childControlRef
        children <- traverseH setupChild $ holes internal

        -- Programmability
        let interpret :: ProgramView
                (WidgetInstr i o i' o' h k)
                (Widget i o a) -> IO ()
            interpret (Return nWidget) = do
                destroyThis
                runWidget' global context controlRef nWidget
            interpret (Await :>>= cont) = writeIORef contRef cont
            interpret (Replace hole nChild :>>= cont) = do
                let childControlRef = getCompose $ getHole hole children
                childControl <- readIORef childControlRef
                destroy childControl
                runWidget' global childContext childControlRef nChild
                interpret $ view $ cont ()
            interpret (SendParent msg :>>= cont) = do
                send context msg
                interpret $ view $ cont ()
            interpret (SendChild msg :>>= cont) = do
                let sendChild msg childControlRef = do
                    childControl <- readIORef $ getCompose childControlRef
                    receive childControl msg
                    return $ Const undefined
                traverseH (sendChild msg) children
                interpret $ view $ cont ()
        let process :: Event i o' k -> IO ()
            process event = do
                cont <- readIORef contRef
                interpret $ view $ cont event
        contRef <- newIORef undefined

        -- Setup control interface
        writeIORef controlRef InstanceControl {
            receive = process . ReceiveParent,
            navigate = undefined, -- TODO
            figure = figureThis,
            destroy = destroyThis }
        redraw context

-- | Runs a widget taking up the full terminal, given a procedure to listen for
-- input messages and a procedure to respond to output messages.
runWidget :: IO i -> (o -> IO ())
    -> Widget i o (Block (Ind Vary Vary)) -> IO ()
runWidget _ output widget = do
    keysRef <- newIORef Map.empty
    let assignKeys struct = forM struct (\(act, pKeys) -> do
        keys <- get
        let tryAdd [] = return Nothing
            tryAdd (pKey : pKeys) = case Map.lookup pKey keys of
                Nothing -> do
                    put $ Map.insert pKey act keys
                    return $ Just pKey
                Just _ -> tryAdd pKeys
        tryAdd pKeys)
    let global = GlobalContext {
        allocKeys = \struct -> do
            keys <- readIORef keysRef
            let (nStruct, nKeys) = runState (assignKeys struct) keys
            writeIORef keysRef nKeys
            return nStruct,
        freeKeys = \keys -> modifyIORef keysRef
            (`Map.difference` (Map.fromList $ map (\x -> (x, ())) keys)),
        focus = undefined } -- TODO
    let inst = InstanceContext {
        send = output,
        redraw = return () }
    controlRef <- newIORef undefined
    runWidget' global inst controlRef widget
    let getSize = do
        Just size' <- Size.size
        return (Size.width size', Size.height size')
    initialSize <- getSize
    sizeRef <- newIORef initialSize
    let getDraw sizeD = do
        control <- readIORef controlRef
        fig <- (<*> pure False) <$> figure control
        return $ fstD (placeD fig <*> plex2D sizeD (pure (0, 0)))
    let initialHeight = snd initialSize
    replicateM_ initialHeight $ putStrLn ""
    cursorUp initialHeight
    initialDraw <- getDraw (set initialSize)
    let initialSt = (fst initialSize, (0, 0), defaultAppearance)
    stRef <- runDraw (final initialDraw) initialSt >>= newIORef
    let redraw = do
        lastSize <- readIORef sizeRef
        curSize <- getSize
        writeIORef sizeRef curSize
        let sizeD = checkD $ stride lastSize curSize
        drawD <- getDraw sizeD
        st <- readIORef stRef
        nSt <- runDraw (paintD drawD) st
        writeIORef stRef nSt
    forever $ do
        key <- getHiddenChar
        -- TODO: input
        -- TODO: navigation and further key processing
        keys <- readIORef keysRef
        fromMaybe (return ()) $ Map.lookup key keys
        redraw
