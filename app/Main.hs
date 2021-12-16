module Main where

import Control.Concurrent.Chan.Unagi.Bounded (newChan)
import Control.Monad (when)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (evalStateT)
import Data.Foldable (asum)
import Data.List (find, isPrefixOf)
import qualified Data.Map.Strict as M
import GHC.Conc (getNumCapabilities)
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Text.Read (readMaybe)

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
          <> value 100000
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

-- | Parse @/proc/cpuinfo@ for the number of physical CPU cores on the system
getNumCores :: IO Int
getNumCores = do
  cpuinfo <- readFile "/proc/cpuinfo"
  case find ("cpu cores" `isPrefixOf`) (lines cpuinfo) of
    Nothing -> do
      putStrLn "Unable to locate CPU core count in /proc/cpuinfo"
      return 0
    Just coreinfo -> case asum $ readMaybe <$> words coreinfo of
      Nothing -> do
        putStrLn "Unable to locate CPU core count in /proc/cpuinfo"
        return 0
      Just n -> return n


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  -- Warn user if more RTS threads than physical cores on system.
  -- Because of the large amount of resource contention between the
  -- worker threads, hyperthreading tends to really hurt performance.
  ncores   <- getNumCores
  nthreads <- getNumCapabilities
  when (ncores > 0 && nthreads > ncores) $ putStrLn
    (  "WARNING: number of runtime threads ("
    ++ show nthreads
    ++ ") exceeds the number of physical CPU cores ("
    ++ show ncores
    ++ ").  "
    ++ "For best performance set '+RTS -N"
    ++ show ncores
    ++ " -RTS'"
    )

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

