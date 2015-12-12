{-# LANGUAGE OverloadedStrings #-}
module Advent.Day12 where

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import qualified Data.Vector as V

jsonSum :: ByteString -> Integer
jsonSum text = truncate (jsonSum' (fromMaybe (Number 0) (decode text)))
  where jsonSum' (Number n) = n
        jsonSum' (Array a)  = V.sum (V.map jsonSum' a)
        jsonSum' (Object o) = sum (map jsonSum' (HM.elems o))
        jsonSum' _          = 0

jsonSumUnred :: ByteString -> Integer
jsonSumUnred text = truncate (jsonSum' (fromMaybe (Number 0) (decode text)))
  where hasRed hm           = elem "red" (HM.elems hm)
        jsonSum' (Number n) = n
        jsonSum' (Array a)  = V.sum (V.map jsonSum' a)
        jsonSum' (Object o)
          | hasRed o        = 0
          | otherwise       = sum (map jsonSum' (HM.elems o))
        jsonSum' _          = 0

jsonSumFile :: FilePath -> IO (Integer, Integer)
jsonSumFile path =
  do bs <- BS.readFile path
     return (jsonSum bs, jsonSumUnred bs)
