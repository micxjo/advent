{-# LANGUAGE OverloadedStrings #-}
module Advent.Day1 where

import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T

santaFloor :: Text -> Int
santaFloor text = (T.count "(" text) - (T.count ")" text)

firstBasement :: Text -> Maybe Int
firstBasement text = L.findIndex (\t -> santaFloor t < 0) (T.inits text)
