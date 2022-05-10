{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module UI where

import Brick
  ( (<=>)
  , App(..)
  , Widget
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

import Types


app :: App State e ()
app = App
  { appDraw         = pure . ui
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = resizeOrQuit
  , appStartEvent   = return
  , appAttrMap      = const styleMap
  }

headerAttr :: A.AttrName
headerAttr = "header"

styleMap :: A.AttrMap
styleMap =
  A.attrMap V.defAttr [(headerAttr, V.black `on` V.white `V.withStyle` V.bold)]

ui :: State -> Widget ()
ui st = vBox
  [ B.borderWithLabel (str "Inputs") $ vLimitPercent 50 $ makeTable Input st
  , B.borderWithLabel (str "Outputs") $ vLimitPercent 50 $ makeTable Output st
  ]

makeTable :: Direction -> State -> Widget ()
makeTable d st = do
  renderTable
    $ surroundingBorder False
    $ columnBorders False
    $ rowBorders False
    $ table (tableHeader : (toRow <$> inputs))
  where inputs = filter (\(_, info) -> ifaceDir info == d) (M.toList st)

tableHeader :: [Widget ()]
tableHeader =
  withAttr headerAttr
    <$> [ padRight (Pad 16) $ str "NAME"
        , padRight (Pad 16) $ str "VLAN"
        , padRight (Pad 10) $ str "THROUGHPUT"
        , padRight (Pad 13) $ str "PACKETS"
        ]

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

humanReadableBytes :: Int -> String
humanReadableBytes n = case find ((< 1024) . fst) pairs of
  Just (val :: Double, fmt) -> printf fmt val
  Nothing                   -> printf "%.0fGiB" (n `div` (1024 ^ 7))
 where
  pairs = zip (iterate (/ 1024) (fromIntegral n)) fmts
  fmts  = ["%.0fB", "%.1fKiB", "%.1fMiB", "%.1fGiB"]
