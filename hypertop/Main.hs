{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick (customMain)
import Brick.BChan (newBChan)
import qualified Graphics.Vty as V
import Data.Sequence (Seq, (|>))
import Data.Time.Clock (UTCTime, getCurrentTime)
import DBus.Client
import Control.Concurrent (forkIO)
import DBus

import DBusClient
import Types
import UI

main :: IO ()
main = do
  -- create D-Bus session and get initial state
  client      <- connectSystem
  ifacesReply <- call_
    client
    (methodCall "/com/hyperpipe/metrics" "com.hyperpipe.metrics" "interfaces")
      { methodCallDestination = Just "com.hyperpipe"
      }
  let state = parseIfacesResponse (head $ methodReturnBody ifacesReply)

  -- create channel and launch thread with D-Bus client
  bchan <- newBChan 8
  forkIO $ runClient bchan client
  
  -- launch Brick app
  let buildVty = V.mkVty V.defaultConfig
  vty <- buildVty
  customMain vty buildVty (Just bchan) app state
  return ()

