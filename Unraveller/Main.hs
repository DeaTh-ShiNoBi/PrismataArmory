module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Tio
import System.Environment
import Data.Char

main = do
  args <- getArgs
  let 
    input = head args
    output = args !! 1
  orig <- Tio.readFile input
  Tio.writeFile output (formatText orig)

formatText :: Text.Text -> Text.Text
formatText t = (
  Text.replace (Text.singleton ',') (Text.singleton '\n')
  . Text.dropWhileEnd (== ',')
  . Text.replace (Text.singleton ']') (Text.singleton ',')
  . Text.replace (Text.singleton '[') Text.empty
  . Text.replace (Text.singleton '.') Text.empty
  . Text.replace (Text.singleton '\'') Text.empty
  . Text.filter (not.isSpace)
  ) t
