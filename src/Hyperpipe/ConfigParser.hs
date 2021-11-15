{-# LANGUAGE OverloadedStrings #-}

-- | Module : ConfigParser
--
-- A module for parsing YAML configuration files that define a `StateModel` (see
-- the "Configuration File" page of the wiki for an example).
module Hyperpipe.ConfigParser
  ( parseCfgData
  , parseCfgFile
  ) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.YAML

import Hyperpipe.EthFrame
import Hyperpipe.StateModel

instance FromYAML StateModel where
  parseYAML = withMap "StateModel" $ \m -> do
    ins  <- m .: "inputs" >>= parseEndpoints Input
    outs <- m .: "outputs" >>= parseEndpoints Output
    return $ StateModel (ins ++ outs)

-- | Attempt to construct a `StateModel` from the contents of a configuration
-- file
parseCfgData :: B.ByteString -> Either String StateModel
parseCfgData cfg =
  let prettyErr (p, e) = prettyPosWithSource p cfg " error" ++ e
  in first prettyErr (decode1 cfg)

-- | Open a config file at the given path and attempt to construct a
-- `StateModel` from its contents
parseCfgFile :: String -> IO (Either String StateModel)
parseCfgFile path = do
  raw <- B.readFile path
  let prettyErr (p, e) = path ++ ":" ++ prettyPosWithSource p raw " error" ++ e
  return $ first prettyErr (decode1 raw)

-- | Parse a list of interfaces as endpoints with a given direction, from a
-- mapping where each key is the name of the interface, and the value is a
-- mapping of options
parseEndpoints :: FlowDir -> Mapping Pos -> Parser [Endpoint]
parseEndpoints dir m = do
  names <- mapM (withStr "string" return) (M.keys m)
  mapM parseIface names
 where
  parseIface n = do
    val <- (m .: n :: Parser (Node Pos))
      -- treat null as empty dict, otherwise parse options mapping
    ops <-
      withNull "null" (return []) val <|> withMap "iface options" parseOps val
    return $ Endpoint (IfaceName $ T.unpack n) dir ops

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

