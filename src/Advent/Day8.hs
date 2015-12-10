{-# LANGUAGE OverloadedStrings #-}
module Advent.Day8 where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.ICU
import           Data.Text.ICU.Replace
import qualified Data.Text.IO as T.IO

codeChars :: Text -> Int
codeChars text = T.length text

uniCharCount :: Text -> Int
uniCharCount text = length (findAll reg text)
  where reg = regex [] "\\\\x[a-fA-F0-9]{2,2}"

escapeCount :: Text -> Int
escapeCount text = length (findAll reg text)
  where reg = regex [] "\\\\(\\\\|\")"

memChars :: Text -> Int
memChars text = (T.length text) - 2 - escapeChars - uniChars
  where escapeChars = escapeCount text
        uniChars = (uniCharCount text) * 3

charCount :: Text -> Int
charCount text = (codeChars text) - (memChars text)

encode :: Text -> Text
encode text = T.concat ["\"", quoteReplace (uniReplace text), "\""]
  where quoteReplace = replaceAll "\"" "\\\""
        uniReplace = replaceAll "\\\\" "\\\\"

encodedCount :: Text -> Int
encodedCount text = (T.length (encode text)) - (codeChars text)

fileCount :: FilePath -> IO (Int, Int)
fileCount path =
  do file <- T.IO.readFile path
     let fileLines = filter (/= T.empty) (T.lines file)
     return (sum (map charCount fileLines), sum (map encodedCount fileLines))
