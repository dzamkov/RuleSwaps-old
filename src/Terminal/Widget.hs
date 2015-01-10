{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Terminal.Widget (
    Context (..),
    Widget (..),
    widget,
    widgetSimple,
    joinWidget,
    GlobalContext (..),
    InstanceContext (..),
    runWidget,
    startTerminal
) where

import Actor
import Delta
import Record (HasRecord, RecordRel1, Void1)
import qualified Record
import Terminal.Input
import Terminal.Draw hiding (fill)
import Terminal.Figure
import Terminal.Page (Page, Direction, Key)
import qualified Terminal.Page as Page
import Data.Functor.Compose
import Data.Traversable
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map as Map
import System.Console.ANSI (setSGR, SGR (Reset))
import qualified System.Console.Terminal.Size as Size
import Control.Monad.Trans
import Control.Monad.State (runState, get, put)
import Control.Monad (forever, when, unless, replicateM_)
import Control.Applicative

-- | Provides context for a widget's actor.
data Context r k = Context {

    -- | A source that is triggered whenever an option on the widget is
    -- selected.
    select :: Source r k,

    -- | An source that is triggered when the widget is destroyed.
    destroy :: Source r () }

-- | Contains the internal information for a widget.
data Internal r rh h k a = Internal {

    -- | The immutable page for this widget.
    page :: Page h k a,

    -- | The initial child widgets to fill the holes for the widget.
    holes :: rh (Widget r),

    -- | The actor which runs while the widget is instantiated. When the
    -- actor terminates, the widget will be replaced by the result.
    actor :: Context r k -> Actor r (Widget r a) }

-- | 'Internal', with existentially-quantified type variables.
data AnyInternal r a = forall rh h k. (RecordRel1 rh h, HasRecord k)
    => AnyInternal (Internal r rh h k a)

-- | A figure-like which enables stateful interactivity.
data Widget r a = Widget (Actor r (AnyInternal r a))

-- | Constructs a widget.
widget :: (RecordRel1 rh h, HasRecord k)
    => Page h k a -- ^ The page the widget is based on
    -> rh (Widget r) -- ^ The initial children for the widget
    -> (Context r k -> Actor r (Widget r a)) -- ^ The program for the widget
    -> Widget r a
widget page holes actor = Widget $ return $ AnyInternal Internal { .. }

-- | Constructs a widget with no child widgets.
widgetSimple :: (HasRecord k)
    => Page Void1 k a -- ^ The page the widget is based on
    -> (Context r k -> Actor r (Widget r a)) -- ^ The program for the widget
    -> Widget r a
widgetSimple page = widget page (Record.gen1 undefined)

-- | Converts an actor that produces a widget into a widget. The actor will
-- be run every time the widget is instantiated.
joinWidget :: Actor r (Widget r a) -> Widget r a
joinWidget builder = Widget (builder >>= (\(Widget w) -> w))

-- | The context information that is shared by all running widgets for a
-- terminal.
data GlobalContext r = GlobalContext {

    -- | Requests assignable keys. Each request consists of a procedure, to
    -- be called when the key is pressed, and a list of possible keys, ordered
    -- by preference. The actual key assignments are returned.
    allocKeys :: forall t. (Traversable t)
        => t (Actor r (), [Key]) -> Actor r (t (Maybe Key)),

    -- | Frees assigned keys so that they may be assigned to other widgets.
    freeKeys :: [Key] -> Actor r () }

-- | The context information needed to run a specific widget.
data InstanceContext r a = InstanceContext {

    -- | Requests a redraw.
    redraw :: Actor r (),

    -- | A source that is triggered when selection changes into or within the
    -- widget. A response of whether selection remains in the widget is
    -- expected.
    navigate :: Source r (Direction, Bool -> Actor r ()),

    -- | A source that is triggered when the widget is being redraw. The
    -- parameter indicates whether any widget has focus. A response of the
    -- figure is expected. All deltas are since the last draw.
    draw :: Source r (Delta Bool, Delta (Figure a) -> Actor r ()),

    -- | A source that is triggered when the widget is about to be destroyed.
    -- This should cause the widget to free its assigned keys and release focus
    -- if it has it.
    destroyInst :: Source r () }

-- | Contains information that can be used to provide an 'InstanceContext'.
data InstanceControl r a = InstanceControl {

    -- | Channel for 'draw'.
    drawChan :: Channel r (Delta Bool, Delta (Figure a) -> Actor r ()),

    -- | Channel for 'destroyInst'.
    destroyInstChan :: Channel r () }

-- | Runs a widget using the given context information. The resulting actor
-- will never terminate.
runWidget :: forall r a. GlobalContext r -> InstanceContext r a
    -> Widget r a -> Actor r ()
runWidget global inst (Widget builder) = do

    -- Instantiate
    AnyInternal internal <- builder

    -- Allocate keys
    selectChan <- spawn
    keyAssignments <- allocKeys global $
        Record.mapWithName (\name keys -> (send selectChan name, keys)) $
        Record.prefFromList $ Page.shortcuts $ page internal

    -- Run children
    invalidateChan <- spawn
    let setupChild :: forall b. Widget r b -> Actor r (InstanceControl r b)
        setupChild child = do
            drawChan <- spawn
            destroyInstChan <- spawn
            let childInst = InstanceContext {
                redraw = send invalidateChan (),
                navigate = undefined, -- TODO: navigation
                draw = source drawChan,
                destroyInst = source destroyInstChan }
            fork $ runWidget global childInst child
            return InstanceControl { .. }
    childControls <- Record.traverse1 setupChild $ holes internal

    -- Figure/drawing
    destroyDrawChan <- spawn
    drawDestroyedChan <- spawn
    let drawListener cache = do
        let getFigureD anyFocusD = do
            let options = Record.map (\key -> plex2D
                  (ifD anyFocusD (keep Nothing) (keep key))
                  (keep False)) keyAssignments
            let drawChild :: forall b. InstanceControl r b
                    -> Actor r (Compose Delta Figure b)
                drawChild child = do
                    responseChan <- spawn
                    send (drawChan child) (anyFocusD, send responseChan)
                    Compose <$> await (source responseChan)
            childFigures <- Record.traverse1 drawChild childControls
            let context = Page.Context {
                getOption = (`Record.get` options),
                getHole = \hole -> getCompose $
                    Record.get1 hole childFigures }
            return $ Page.figure (page internal) context
        msg <- await $
            (Left False <$ source destroyDrawChan) <>
            (Left True <$ source invalidateChan) <>
            (Right <$> draw inst)
        case msg of
            Left False -> send drawDestroyedChan ()
            Left True -> do
                redraw inst
                drawListener (Left False)
            Right (anyFocusD, respond) -> case (anyFocusD, cache) of
                (Keep _, Right cache) -> do
                    respond $ keep cache
                    drawListener (Right cache)
                (anyFocusD, Right _) -> do
                    figureD <- getFigureD anyFocusD
                    respond figureD
                    drawListener (Right $ final figureD)
                (anyFocusD, Left firstTime) -> do
                    figureD <- getFigureD anyFocusD
                    respond $ if firstTime
                        then set (final figureD)
                        else figureD
                    drawListener (Right $ final figureD)
    fork $ drawListener (Left True)

    -- Destruction
    destroyChan <- spawn
    replaceChan <- spawn
    replaceOkayChan <- spawn
    let destroyListener = do
        external <- await $
            (True <$ destroyInst inst) <>
            (False <$ source replaceChan)
        freeKeys global $ catMaybes $ Record.elems keyAssignments
        send destroyDrawChan ()
        let destroyChild child = do
            send (destroyInstChan child) ()
            return child
        Record.traverse1 destroyChild childControls
        when external $ send destroyChan ()
        await $ source drawDestroyedChan
        unless external $ send replaceOkayChan True
        forever $ do
            await $ source replaceChan
            send replaceOkayChan False
    fork destroyListener

    -- Programmability
    let context = Context {
        select = source selectChan,
        destroy = source destroyChan }
    nWidget <- actor internal context

    -- Replace with resulting widget
    send replaceChan ()
    okay <- await $ source replaceOkayChan
    when okay $ runWidget global inst nWidget

-- | Describes a request for keys, used to implement 'allocKeys'.
data KeyRequest r = forall t. (Traversable t) =>
    KeyRequest (t (Actor r (), [Key])) (t (Maybe Key) -> Actor r ())

-- | Prepares a terminal to run a widget. The given source can be used to
-- release the terminal.
startTerminal :: Source IOContext ()
    -> ActorT IOContext IO (
        GlobalContext IOContext,
        InstanceContext IOContext (Block (Ind Vary Vary)))
startTerminal close = do

    -- Closing/destruction
    stopKeyChan <- spawn
    stopDrawChan <- spawn
    destroyInstChan <- spawn
    fork $ do
        await close
        send stopKeyChan ()
        send stopDrawChan ()
        send destroyInstChan ()
        liftIO $ do
            setSGR [Reset]
            putStrLn ""

    -- Key assignment
    allocChan <- spawn
    freeChan <- spawn
    keyChan <- spawn
    let keyAssignListener keys = do
        msg <- await $
            (Left . Left <$> source allocChan) <>
            (Left . Right <$> source freeChan) <>
            (Right <$> source keyChan)
        case msg of
            Left (Left (KeyRequest struct respond)) -> do
                let assignKeys = forM struct (\(act, pKeys) -> do
                    keys <- get
                    let tryAdd [] = return Nothing
                        tryAdd (pKey : pKeys) = case Map.lookup pKey keys of
                            Nothing -> do
                                put $ Map.insert pKey act keys
                                return $ Just pKey
                            Just _ -> tryAdd pKeys
                    tryAdd pKeys)
                let (nStruct, nKeys) = runState assignKeys keys
                liftActor $ respond nStruct
                keyAssignListener nKeys
            Left (Right freeKeys) -> do
                let tKeys = Map.fromList $ map (\x -> (x, ())) freeKeys
                let nKeys = Map.difference keys tKeys
                keyAssignListener nKeys
            Right key -> do
                liftActor $ fromMaybe (return ()) $ Map.lookup key keys
                keyAssignListener keys
    let allocKeys :: forall t. (Traversable t)
            => t (Actor IOContext (), [Key]) -> Actor IOContext (t (Maybe Key))
        allocKeys struct = do
            responseChan <- spawn
            send allocChan (KeyRequest struct (send responseChan))
            await $ source responseChan
    let freeKeys = send freeChan
    fork $ keyAssignListener Map.empty

    -- Key input
    let keyListener = do
        key <- awaitBlocking getHiddenChar (source stopKeyChan)
        case key of
            Left key -> do
                send keyChan key
                keyListener
            Right _ -> return ()
    fork keyListener

    -- Drawing
    redrawChan <- spawn
    drawChan <- spawn
    let getSize = do
        Just size' <- Size.size
        return (Size.width size', Size.height size')
    let getDraw (sizeD :: Delta (Int, Int)) = do
        responseChan <- spawn
        send drawChan (pure False, \figD ->
            send responseChan figD :: Actor IOContext ())
        figD <- await $ source responseChan
        return $ fstD (placeD figD <*> plex2D sizeD (pure (0, 0)))
    let initialDraw = do
        size <- liftIO getSize
        draw <- getDraw $ set size
        liftIO $ do
            let height = snd size
            replicateM_ height $ putStrLn ""
            cursorUp height
            let st = (fst size, (0, 0), defaultAppearance)
            nSt <- runDraw (final draw) st
            return (size, nSt)
    let reDraw (size, st) = do
        nSize <- liftIO getSize
        draw <- getDraw $ checkD $ stride size nSize
        nSt <- liftIO $ runDraw (paintD draw) st
        return (nSize, nSt)
    let initialDrawListener = do
            state <- initialDraw
            drawListener state
        drawListener state = do
            msg <- await $
                (True <$ source redrawChan) <>
                (False <$ source stopDrawChan)
            when msg $ do
                nState <- reDraw state
                drawListener nState
    fork initialDrawListener

    -- Contexts
    let redraw = send redrawChan ()
    let navigate = undefined -- TODO
    let draw = source drawChan
    let destroyInst = source destroyInstChan
    return (GlobalContext { .. }, InstanceContext { .. })
