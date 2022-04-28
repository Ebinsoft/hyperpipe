module Main where

import Control.Applicative (optional)
import Control.Concurrent.Chan.Unagi.Bounded (newChan)
import Control.Monad (when)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (evalStateT)
import Data.Char (toLower)
import Data.Foldable (asum)
import Data.List (find, isPrefixOf)
import qualified Data.Map.Strict as M
import Data.Version (showVersion)
import GHC.Conc (setNumCapabilities)
import Options.Applicative
import Paths_hyperpipe (version)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Text.Read (readMaybe)

import Hyperpipe

data Options = Options
  { optConfig   :: String
  , optLogLevel :: LogLevel
  , optThreads  :: Maybe Int
  , optTimeout  :: Int
  , optQueueLen :: Int
  }

versionOpt :: Parser (a -> a)
versionOpt = infoOption
  (showVersion version)
  (long "version" <> help "Show version number")

options :: Parser Options
options =
  Options
    <$> argument str (metavar "FILE" <> help "YAML configuration file")
    <*> option
          readLogLevel
          (  long "log-level"
          <> short 'l'
          <> metavar "LEVEL"
          <> showDefault
          <> value INFO
          <> help "Minimum severity level of log messages that are displayed."
          )
    <*> optional
          (option
            readThreadCount
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
          readTimeout
          (  long "timeout"
          <> metavar "INT"
          <> showDefault
          <> value 100000
          <> help
              "The packet buffer timeout for each capture device (in microseconds). \
              \See https://www.tcpdump.org/manpages/pcap.3pcap.html for an explanation of the packet buffer timeout."
          )
    <*> option
          readQueueLen
          (  long "queue"
          <> metavar "INT"
          <> showDefault
          <> value 100
          <> help
              "Number of packets to hold in the internal queue between input and output interfaces."
          )

readLogLevel :: ReadM LogLevel
readLogLevel = str >>= \s -> case toLower <$> s of
  "debug" -> return DEBUG
  "info"  -> return INFO
  "warn"  -> return WARN
  "error" -> return ERROR
  _ ->
    readerError "Accepted log levels are 'debug', 'info', 'warn', and 'error'."

readThreadCount :: ReadM Int
readThreadCount = auto >>= \i ->
  if i < 1 then readerError "Thread count must be positive." else return i

readTimeout :: ReadM Int
readTimeout = auto >>= \i -> if i < 0
  then readerError "Packet buffer timeout cannot be negative."
  else return i

readQueueLen :: ReadM Int
readQueueLen = auto >>= \i ->
  if i < 1 then readerError "Queue length must be positive." else return i

optInfo :: ParserInfo Options
optInfo =
  info (helper <*> versionOpt <*> options) (fullDesc <> header "HYPERPIPE")

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

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  opts <- execParser optInfo
  res  <- parseCfgFile (optConfig opts)
  case res of
    Left  err   -> putStrLn err >> exitFailure
    Right model -> do
      (inChn, outChn) <- newChan (optQueueLen opts)

      logHandle       <- makeLogger (optLogLevel opts)
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

