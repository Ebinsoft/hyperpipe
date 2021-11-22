module Main where

import Control.Concurrent (newChan)
import Control.Monad (when)
import Control.Monad.State.Strict (evalStateT)
import qualified Data.Map.Strict as M
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Hyperpipe


main :: IO ()
main = do
  args <- getArgs
  when (null args) (putStrLn "gib config file" >> exitFailure)

  res <- parseCfgFile (head args)
  case res of
    Left  err   -> putStrLn err >> exitFailure
    Right model -> do
      chn <- newChan
      evalStateT (runWithConfig model) (chn, M.empty)

