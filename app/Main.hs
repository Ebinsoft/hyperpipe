{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative (optional)
import Control.Concurrent.Chan.Unagi.Bounded (newChan)
import Control.Monad (when)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (evalStateT)
import Data.Foldable (asum)
import Data.List (find, isPrefixOf)
import qualified Data.Map.Strict as M
import GHC.Conc (setNumCapabilities)
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Text.Read (readMaybe)

import Hyperpipe

data Options = Options
  { optConfig   :: String
  , optDebug    :: Bool
  , optThreads  :: Maybe Int
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
    <*> optional
          (option
            auto
            (  long "threads"
            <> short 't'
            <> metavar "INT"
            <> help
                "Number of OS-level threads to deploy for the runtime system.  \
                \By default hyperpipe will automatically create as many as it \
                \needs without exceeding the number of physical CPU cores."
            )
          )
    <*> option
          auto
          (  long "timeout"
          <> metavar "INT"
          <> showDefault
          <> value 100000
          <> help
              "The packet buffer timeout for each capture device (in microseconds). \
              \See https://www.tcpdump.org/manpages/pcap.3pcap.html for an explanation of the packet buffer timeout."
          )
    <*> option
          auto
          (  long "queue"
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
  case
      find ("cpu cores" `isPrefixOf`) (lines cpuinfo)
        >>= (asum . (readMaybe <$>) . words)
    of
      Nothing -> do
        putStrLn "Unable to locate CPU core count in /proc/cpuinfo"
        return 0
      Just n -> return n

-- | Ensure the user didn't give any bad values for the command-line options
validateOpts :: Options -> IO Options
validateOpts o@Options {..} = do
  case optThreads of
    Nothing -> return ()
    Just t  -> when (t < 1) $ do
      putStrLn $ "Invalid number of threads: " ++ show t
      exitFailure

  when (optTimeout < 0) $ do
    putStrLn $ "Invalid packet buffer timeout: " ++ show optTimeout
    exitFailure

  when (optQueueLen < 1) $ do
    putStrLn $ "Invalid queue length: " ++ show optQueueLen
    exitFailure

  return o

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  opts <- execParser optInfo >>= validateOpts
  res  <- parseCfgFile (optConfig opts)
  case res of
    Left  err   -> putStrLn err >> exitFailure
    Right model -> do
      (inChn, outChn) <- newChan (optQueueLen opts)

      logHandle       <- makeLogger DEBUG
      -- set the number of runtime capabilities
      ncores          <- getNumCores
      case optThreads opts of
        Just n -> do
          when (n > ncores)
            $ withLogger logHandle
            $ logWarn
                "Number of threads exceeds the number of physical CPU cores. \
                \This will likely hurt performance."
          setNumCapabilities n
        Nothing -> setNumCapabilities $ min ncores (1 + numEndpoints model)

      let
        env = Env
          { bufTimeout = optTimeout opts
          , queueIn    = inChn
          , queueOut   = outChn
          , logger     = logHandle
          }
      evalStateT (runReaderT (runWithModel model) env) M.empty

