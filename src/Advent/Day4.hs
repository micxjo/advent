{-# LANGUAGE OverloadedStrings #-}
module Advent.Day4 where

import qualified Crypto.Hash.MD5 as MD5
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Hex
import qualified Data.List as L

hash :: ByteString -> Int -> ByteString
hash key num = hex (MD5.hash (key `BS.append` BS.pack (show num)))

findHash :: ByteString -> Int -> Maybe Int
findHash key zeroCount =
  (+1) <$> L.findIndex (BS.isPrefixOf prefix) (map (hash key) [1..])
  where prefix = BS.pack (take zeroCount (repeat '0'))
