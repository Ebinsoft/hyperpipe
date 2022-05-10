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
  client      <- connectSession

  ifacesReply <- call_
    client
    (methodCall "/com/hyperpipe/metrics" "com.hyperpipe.metrics" "interfaces")
      { methodCallDestination = Just "com.hyperpipe"
      }

  let state = parseIfacesResponse (head $ methodReturnBody ifacesReply)

  bchan <- newBChan 8

  forkIO $ runClient bchan client
  
  let buildVty = V.mkVty V.defaultConfig
  vty <- buildVty
  customMain vty buildVty (Just bchan) app state
  return ()

