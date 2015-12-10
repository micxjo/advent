{-# LANGUAGE OverloadedStrings #-}
module Advent.Day10 where

import           Data.Text (Text)
import qualified Data.Text as T

lookAndSayGroup :: Text -> Text
lookAndSayGroup text = (T.pack (show count)) `T.append` (T.singleton digit)
  where digit = T.head text
        count = T.length text

lookAndSay :: Text -> Text
lookAndSay text = T.concat (map lookAndSayGroup (T.group text))

iterativeLookAndSay :: Text -> Int -> Int
iterativeLookAndSay text count = T.length ((iterate lookAndSay text) !! count)
