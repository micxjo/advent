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

countMinCombs :: [Int] -> Int -> Int
countMinCombs is i = length (filter (\(l, _) -> l == minLen) mins)
  where combs    = concatMap (combinations is) [0..(length is)]
        combCaps = map (\c -> (length c, sum c)) combs
        mins     = filter (\(_, s) -> s == i) combCaps
        minLen   = minimum (map fst mins)
