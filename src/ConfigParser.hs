{-# LANGUAGE OverloadedStrings #-}

module ConfigParser where

import Control.Applicative ((<|>))
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.YAML

import EthFrame
import StateModel

-- | Intermediate representation for `StateModel` that is parsed from YAML file
data ModelCfg = ModelCfg
  { inputs  :: [IfaceCfg]
  , outputs :: [IfaceCfg]
  }
  deriving Show

-- | Individual network interface defined in YAML config file
data IfaceCfg = IfaceCfg
  { ifName :: String
  , ifOps  :: [FrameOp]
  }
  deriving Show

instance FromYAML ModelCfg where
  parseYAML = withMap "ModelCfg" $ \m -> do
    ins  <- m .: "inputs" >>= parseIfaces
    outs <- m .: "outputs" >>= parseIfaces
    return $ ModelCfg ins outs

-- | Parse a list of interfaces from a mapping where each key is the name of the
-- interface, and the value is a mapping of options
parseIfaces :: Mapping Pos -> Parser [IfaceCfg]
parseIfaces m = do
  names <- mapM (withStr "string" return) (M.keys m)
  mapM parseIface names
 where
  parseIface n = do
    val  <- (m .: n :: Parser (Node Pos))
    -- treat null as empty dict, otherwise parse options mapping
    opts <-
      withNull "null" (return []) val <|> withMap "iface options" parseOps val
    return $ IfaceCfg (T.unpack n) opts

-- | Parse a list of frame operations from a YAML mapping
parseOps :: Mapping Pos -> Parser [FrameOp]
parseOps m = do
  vlan <- parseVLANOp m
  -- this is kind of unnecessary right now, but if we add more optional settings
  -- in the future, this will make it easy to parse them all out together
  return $ catMaybes [vlan]

-- | Specifically parse out the "vlan" option from a YAML mapping, if it exists
parseVLANOp :: Mapping Pos -> Parser (Maybe FrameOp)
parseVLANOp m = case parseEither (m .: "vlan" :: Parser (Node Pos)) of
  Left  _ -> return Nothing
  Right n -> withNull "null" stripV n <|> withInt "VLAN tag" setV n
 where
  stripV = return (Just StripVLAN)
  setV i = return (Just (SetVLAN (VLANTag $ fromIntegral i)))

