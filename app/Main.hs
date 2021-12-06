module Main where

import Control.Concurrent (newChan)
import Control.Monad (when)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (evalStateT)
import qualified Data.Map.Strict as M
import Options.Applicative
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Hyperpipe

-- | Data structure holding all possible command-line options for the program
data Options = Options
  { optConfig :: String
  , optDebug  :: Bool
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


optInfo :: ParserInfo Options
optInfo = info
  (options <**> helper)
  (fullDesc <> header "HYPERPIPE :D")


main :: IO ()
main = do
  opts <- execParser optInfo
  res  <- parseCfgFile (optConfig opts)
  case res of
    Left  err   -> putStrLn err >> exitFailure
    Right model -> do
      chn <- newChan
      let settings = Settings { debugMode = optDebug opts }
      evalStateT (runReaderT (runWithModel model) settings) (chn, M.empty)

