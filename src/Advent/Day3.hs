{-# LANGUAGE OverloadedStrings #-}
module Advent.Day3 where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

type Position = (Int, Int)

move :: Position -> Char -> Position
move (x, y) '^' = (x, y + 1)
move (x, y) 'v' = (x, y - 1)
move (x, y) '>' = (x + 1, y)
move (x, y) '<' = (x - 1, y)
move (x, y) _   = (x, y)

deliveries :: Text -> Set Position
deliveries text = Set.fromList positions
  where nextPosition c (pos:_) = move pos c
        nextPosition _ _       = (0, 0)
        positions = T.foldl (\acc c -> nextPosition c acc : acc) [(0, 0)] text

countDeliveries :: Text -> Int
countDeliveries = length . deliveries

countDeliveriesFromFile :: FilePath -> IO Int
countDeliveriesFromFile path = T.IO.readFile path >>= return . countDeliveries

splitDeliveries :: Text -> (Text, Text)
splitDeliveries text = (T.pack (reverse santa), T.pack (reverse roboSanta))
  where split' (f, s) [] = (f, s)
        split' (f, s) (c:rest) = split' (c:s, f) rest
        (santa, roboSanta) = split' ("", "") (T.unpack text)

roboDeliveries :: Text -> Set Position
roboDeliveries text = (deliveries santa) `Set.union` (deliveries roboSanta)
  where (santa, roboSanta) = splitDeliveries text

countRoboDeliveries :: Text -> Int
countRoboDeliveries = length . roboDeliveries

countRoboDeliveriesFromFile :: FilePath -> IO Int
countRoboDeliveriesFromFile path =
  T.IO.readFile path >>= return . countRoboDeliveries
