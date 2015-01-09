{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Terminal.UI (
    Style (..),
    casino,
    MainContext (..),
    main
) where

import Actor
import Record (Record, EnumRecord)
import Terminal.Draw (Color (..), ColorIntensity (..), FullColor)
import Terminal.Figure
import Terminal.Page hiding (Context)
import Terminal.Widget

-- | Contains styling information for the user interface.
data Style = Style {

    -- | The color used to show assigned keys.
    keyColor :: FullColor,

    -- | The color used for header text.
    headerColor :: FullColor,

    -- | The color used for normal text.
    textColor :: FullColor,

    -- | The color used for highlighted (selected) text.
    highlightColor :: FullColor,

    -- | The color used for the screen background.
    screenBack :: FullColor,

    -- | The border applied to floating objects.
    floatBorder :: Border,

    -- | The back color for floating objects.
    floatBack :: FullColor }

-- | A style that simulates a casino-style poker table.
casino :: Style
casino = Style {
    keyColor = (Vivid, Blue),
    headerColor = (Vivid, Red),
    textColor = (Dull, Black),
    highlightColor = (Vivid, Yellow),
    screenBack = (Dull, Green),
    floatBorder = padding (Dull, White) (1, 1, 1, 1),
    floatBack = (Vivid, White) }

-- | Creates a header.
header :: (?style :: Style) => String -> Page h k Flow
header = figureToPage . tightText (headerColor ?style)

-- | Creates an option that can be selected.
button :: (?style :: Style) => k -> [Key] -> String -> Page h k Flow
button id keys name = res where
    Style { .. } = ?style
    view = keyView id (\key -> case key of
        Just key -> tightText keyColor ("[" ++ (key : "") ++ "]") +++ space 1
        Nothing -> empty)
    strongSpace' = figureToPage . strongSpace
    label = figureToPage $ text textColor name
    res = option id keys (highlightFlow highlightColor)
        (strongSpace' 1 +++ view +++ label +++ strongSpace' 1)

-- | Identifies an option on the main menu.
data MenuOption = Join | Host | Exit deriving (Eq, Ord, Enum, Bounded)
type instance Record MenuOption = EnumRecord MenuOption

-- | The context information needed by the UI when there is no active game.
data MainContext r = MainContext {

    -- | Exits the program.
    exit :: Actor r () }

-- | The widget for the entire user interface.
{-# ANN module "HLint: ignore Use string literal" #-}
main :: (?style :: Style) => MainContext r -> Widget r (Block (Ind Vary Vary))
main (MainContext { .. }) = res where
    page = center $
        pad (screenBack ?style) (5, 5, 5, 5) $
        withBorder (floatBorder ?style) $
        pad (floatBack ?style) (2, 2, 2, 2) $
        setWidth 12 $
        blockify (floatBack ?style)
            (header "RuleSwaps"
            =====
            button Join ['j'] "Join"
            ===
            button Host ['h'] "Host"
            ===
            button Exit ['e'] "Exit")
    actor (Context { .. }) = do
        option <- await select
        case option of
            Join -> undefined -- TODO
            Host -> undefined -- TODO
            Exit -> exit >> return res
    res = widgetSimple page actor
