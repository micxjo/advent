{-# LANGUAGE OverloadedStrings #-}
module Advent.Day9 where

import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Char
import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text.IO as T.IO

type Link = ((Text, Text), Int)

link :: Parser Link
link =
  do city1 <- takeTill isSpace
     _ <- string " to "
     city2 <- takeTill isSpace
     _ <- string " = "
     distance <- decimal
     _ <- many' endOfLine
     pure ((city1, city2), distance)

parseLinks :: Text -> [Link]
parseLinks text = either (const []) id (parseOnly (many1' link) text)

type Graph = Map (Text, Text) Int

graphFromFile :: FilePath -> IO Graph
graphFromFile path =
  do file <- T.IO.readFile path
     let links = parseLinks file
     let kv ((c1, c2), dist) = [((c1, c2), dist), ((c2, c1), dist)]
     return (M.fromList (concatMap kv links))

cities :: Graph -> [Text]
cities graph = L.nub (map fst (M.keys graph))

type Path = [(Text, Text)]

makePath :: [Text] -> Path
makePath cs = zip cs (tail cs)

pathDistance :: Graph -> Path -> Int
pathDistance graph path = sum (map distance path)
  where distance c = M.findWithDefault (maxBound :: Int) c graph

findPaths :: FilePath -> IO (Int, Int)
findPaths path =
  do graph <- graphFromFile path
     let starts = cities graph
     let perms = L.permutations starts
     let paths = map makePath perms
     let distances = map (pathDistance graph) paths
     return ((minimum distances), (maximum distances))
