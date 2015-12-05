{-# LANGUAGE OverloadedStrings #-}
module Advent.Day5 where

import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

isVowel :: Char -> Bool
isVowel c = c `elem` ("aeiou" :: String)

bigrams :: [a] -> [(a, a)]
bigrams (a:b:rest) = (a, b) : bigrams (b:rest)
bigrams _ = []

isNice :: Text -> Bool
isNice text = vowelCount >= 3 && repeats > 0 && (not hasBadString)
  where string = T.unpack text
        vowelCount = length (L.findIndices isVowel string)
        repeats = length (filter (uncurry (==)) (bigrams string))
        hasBadString = any (`T.isInfixOf` text) ["ab", "cd", "pq", "xy"]

niceFile :: FilePath -> IO Int
niceFile path =
  T.IO.readFile path >>= return . length . filter isNice . T.lines
