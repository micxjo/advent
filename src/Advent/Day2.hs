{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Advent.Day2 where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

wrappingPaper :: Int -> Int -> Int -> Int
wrappingPaper l w h = sum sides + (minimum sides `div` 2)
  where sides = [2 * l * w, 2 * w * h, 2 * h * l]

dimensionsFromFile :: FilePath -> IO [(Int, Int, Int)]
dimensionsFromFile path =
  do file <- T.IO.readFile path
     let boxes = T.lines file
     let dims = map (\l -> map (read . T.unpack) (T.splitOn "x" l)) boxes
     return (map (\case
                          [l, w, h] -> (l, w, h)
                          _         -> (0, 0, 0))
                  dims)

wrappingPaperFromFile :: FilePath -> IO Int
wrappingPaperFromFile path =
  do dims <- dimensionsFromFile path
     return (sum (map (\(l, w, h) -> wrappingPaper l w h) dims))

ribbon :: Int -> Int -> Int -> Int
ribbon l w h = minimum perimeters + l * w * h
  where perimeters = [2 * l + 2 * w, 2 * w + 2 * h, 2 * h + 2 * l]

ribbonFromFile :: FilePath -> IO Int
ribbonFromFile path =
  do dims <- dimensionsFromFile path
     return (sum (map (\(l, w, h) -> ribbon l w h) dims))

