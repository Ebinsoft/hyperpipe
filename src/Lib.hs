module Lib
  ( someFunc
  ) where

someFunc :: IO ()
someFunc = putStrLn . unlines $ drop <> take <$> [0 .. 6] <*> pure "POGGERS"
