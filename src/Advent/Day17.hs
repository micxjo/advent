{-# LANGUAGE OverloadedStrings #-}
module Advent.Day17 where

import Data.List (tails)

combinations :: [Int] -> Int -> [[Int]]
combinations _ 0 = [[]]
combinations xs n = [y:ys | y:xs' <- tails xs
                          , ys <- combinations xs' (n - 1)]

countCombs :: [Int] -> Int -> Int
countCombs is i = length (filter (\s -> sum s == i)
                          (concatMap (combinations is) [0..(length is)]))
