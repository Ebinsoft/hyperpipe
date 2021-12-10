module Main where

import Control.Concurrent.Chan.Unagi.Bounded (newChan)
import Control.Monad (when)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (evalStateT)
import qualified Data.Map.Strict as M
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

import Hyperpipe

-- | Data structure holding all possible command-line options for the program
data Options = Options
  { optConfig   :: String
  , optDebug    :: Bool
  , optTimeout  :: Int
  , optQueueLen :: Int
  }

options :: Parser Options
options =
  Options
    <$> argument str (metavar "FILE" <> help "YAML configuration file")
    <*> switch
          (  long "debug"
          <> short 'd'
          <> help
              "When debug mode is active, each received packet is printed to stdout"
          )
    <*> option
          auto
          (  long "timeout"
          <> short 't'
          <> metavar "INT"
          <> showDefault
          <> value 1000
          <> help
              "The packet buffer timeout for each capture device (in microseconds). \
              \See https://www.tcpdump.org/manpages/pcap.3pcap.html for an explanation of the packet buffer timeout."
          )
    <*> option
          auto
          (  long "queue-size"
          <> short 'q'
          <> metavar "INT"
          <> showDefault
          <> value 100
          <> help
              "Number of packets to hold in the internal queue between input and output interfaces."
          )

optInfo :: ParserInfo Options
optInfo = info (options <**> helper) (fullDesc <> header "HYPERPIPE :D")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  opts <- execParser optInfo
  res  <- parseCfgFile (optConfig opts)
  case res of
    Left  err   -> putStrLn err >> exitFailure
    Right model -> do
      (inChn, outChn) <- newChan (optQueueLen opts)
      let
        settings =
          Settings { debugMode = optDebug opts, bufTimeout = optTimeout opts }
      evalStateT
        (runReaderT (runWithModel model) settings)
        (inChn, outChn, M.empty)

