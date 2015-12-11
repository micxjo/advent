{-# LANGUAGE OverloadedStrings #-}
module Advent.Day11 where

import qualified Data.Char as C
import qualified Data.List as L

increment :: String -> String
increment str = reverse (increment' (reverse str))
  where increment' ('z':rest) = 'a' : increment' rest
        increment' (c:rest)   = nextChar c : rest
        increment' ""         = ""
        nextChar c            = C.chr ((C.ord c) + 1)

hasIncreasing :: String -> Bool
hasIncreasing (a:b:c:rest) =
  (b' == a' + 1 && c' == b' + 1) || hasIncreasing (b:c:rest)
  where a' = C.ord a
        b' = C.ord b
        c' = C.ord c
hasIncreasing _ = False

hasBadChar :: String -> Bool
hasBadChar str =
  "i" `L.isInfixOf` str || "o" `L.isInfixOf` str || "l" `L.isInfixOf` str

hasPairs :: String -> Bool
hasPairs str =
  (length (filter (\g -> (length g) >= 2) (L.group str))) >= 2

goodPassword :: String -> Bool
goodPassword pass =
  hasIncreasing pass && not (hasBadChar pass) && hasPairs pass

nextPassword :: String -> String
nextPassword pass =
  head (filter goodPassword (iterate increment pass))
