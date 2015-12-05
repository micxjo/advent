{-# LANGUAGE OverloadedStrings #-}
module Advent.Day5 where

import qualified Data.List as L
import           Data.MultiMap (MultiMap)
import qualified Data.MultiMap as MM
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

isVowel :: Char -> Bool
isVowel c = c `elem` ("aeiou" :: String)

bigrams :: [a] -> [(a, a)]
bigrams ls = zip ls (tail ls)

trigrams :: [a] -> [(a, a, a)]
trigrams ls = zip3 ls (tail ls) (drop 2 ls)

isNice :: Text -> Bool
isNice text = vowelCount >= 3 && repeats > 0 && not hasBadString
  where string = T.unpack text
        vowelCount = length (L.findIndices isVowel string)
        repeats = length (filter (uncurry (==)) (bigrams string))
        hasBadString = any (`T.isInfixOf` text) ["ab", "cd", "pq", "xy"]

niceFile :: FilePath -> IO Int
niceFile path =
  T.IO.readFile path >>= return . length . filter isNice . T.lines

bigramIndices :: Ord a => [a] -> MultiMap (a, a) Int
bigramIndices string = MM.fromList (zip (bigrams string) [0..])

isNiceNew :: Text -> Bool
isNiceNew text = goodBigram && goodTrigram
  where string = T.unpack text
        nonOverlapping cs = filter (\l -> length l == 1)
                                   (L.groupBy (\a b -> b == a + 1) cs)
        goodBigram = any (\l -> length (nonOverlapping l) > 1)
                         (MM.elems (bigramIndices string))
        goodTrigram = any (\(a, _, c) -> a == c) (trigrams string)

niceNewFile :: FilePath -> IO Int
niceNewFile path =
  T.IO.readFile path >>= return . length . filter isNiceNew . T.lines
