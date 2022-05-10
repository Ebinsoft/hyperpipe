{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Module: UI
--
-- Defines the structure and behavior of the Brick-driven TUI for hypertop.
module UI where

import Brick
  ( (<=>)
  , App(..)
  , BrickEvent(..)
  , EventM
  , Next
  , Widget
  , continue
  , halt
  , neverShowCursor
  , resizeOrQuit
  , str
  , vLimitPercent
  , withAttr
  )
import qualified Brick.AttrMap as A
import Brick.Types (Padding(..))
import Brick.Util (on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
import Brick.Widgets.Table
import Data.List (find)
import qualified Data.Map as M
import Data.Sequence (Seq(..))
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes (Attr(..))
import Text.Printf (printf)

import DBusClient
import Types

app :: App State Event ()
app = App
  { appDraw         = pure . ui
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const styleMap
  }

handleEvent :: State -> BrickEvent () Event -> EventM () (Next State)
handleEvent s (AppEvent (UpdateInfo time infoMap)) =
  continue (updateState s time infoMap)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleEvent s _ = continue s

headerAttr :: A.AttrName
headerAttr = "header"

styleMap :: A.AttrMap
styleMap =
  A.attrMap V.defAttr [(headerAttr, V.black `on` V.white `V.withStyle` V.bold)]

-- | Creates the input and output tables of interfaces from the `State`
ui :: State -> Widget ()
ui st = vBox $ C.hCenter <$>
  [ epicTitle
  , B.borderWithLabel (str "Inputs") $ vLimitPercent 50 $ makeTable Input st
  , B.borderWithLabel (str "Outputs") $ vLimitPercent 50 $ makeTable Output st
  , str "Press Q or Esc to exit."
  ]

epicTitle :: Widget ()
epicTitle = 
  str
    "     __                          __            \n \
    \   / /_  __  ______  ___  _____/ /_____  ____ \n \
    \  / __ \\/ / / / __ \\/ _ \\/ ___/ __/ __ \\/ __ \\\n \
    \ / / / / /_/ / /_/ /  __/ /  / /_/ /_/ / /_/ /\n \
    \/_/ /_/\\__, / .___/\\___/_/   \\__/\\____/ .___/ \n \
    \      /____/_/                       /_/      "

-- | Creates an info table for interfaces of a specific `Direction` contained
-- within the `State`.
makeTable :: Direction -> State -> Widget ()
makeTable d st = do
  renderTable
    $ surroundingBorder False
    $ columnBorders False
    $ rowBorders False
    $ table (tableHeader : (toRow <$> inputs))
  where inputs = filter (\(_, info) -> ifaceDir info == d) (M.toList st)

-- | Fixed header of info table
tableHeader :: [Widget ()]
tableHeader =
  withAttr headerAttr
    <$> [ padRight (Pad 16) $ str "NAME"
        , padRight (Pad 16) $ str "VLAN"
        , padRight (Pad 10) $ str "THROUGHPUT"
        , padRight (Pad 13) $ str "PACKETS"
        ]

-- | Converts an interface name and `IfaceInfo` pair into a row of a Brick table
toRow :: (String, IfaceInfo) -> [Widget ()]
toRow (name, IfaceInfo {..}) =
  [nameCol name, vlanCol vlanSetting] ++ throughput
 where
  throughput = case usageHistory of
    Empty                  -> [bytesCol 0, pktsCol 0]
    _ :|> (_, pkts, bytes) -> [bytesCol bytes, pktsCol pkts]
  nameCol  = str
  vlanCol  = str . (\s -> if null s then " " else s)
  pktsCol  = str . (++ "pps") . show
  bytesCol = str . (++ "ps") . humanReadableBytes

-- | Renders a quantity of bytes as a string using the largest unit applicable.
-- 
-- __Example__
-- >>> humanReadableBytes 2345
-- "2.3KiB"
humanReadableBytes :: Int -> String
humanReadableBytes n = case find ((< 1024) . fst) pairs of
  Just (val :: Double, fmt) -> printf fmt val
  Nothing                   -> printf "%.0fGiB" (n `div` (1024 ^ 7))
 where
  pairs = zip (iterate (/ 1024) (fromIntegral n)) fmts
  fmts  = ["%.0fB", "%.1fKiB", "%.1fMiB", "%.1fGiB"]
