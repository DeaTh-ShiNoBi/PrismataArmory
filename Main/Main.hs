{-# language ScopedTypeVariables #-}

module Main where

import Control.Monad (unless)
import System.IO
import System.Environment (getArgs)
import qualified Data.Set as Set
import Data.Char
import Data.List

prepIn :: String -> String
prepIn [] = ""
prepIn s = 
  let
    x :: String
    x = (map toLower . dropWhileEnd isSpace) s

    ss :: [String]
    ss = words x

    list :: [String]
    list = "2r":"3r":"5r":"10r"
          :"2b":"3b":"5b":"10b"
          :"2g":"3g":"5g":"10g"
          :[]
  in
    if (any (`elem` x) "rbg")
      then map (boolToChar . (`elem` ss)) list
      else concat ss

boolToChar :: Bool -> Char
boolToChar a = if a then '1' else '0'

main :: IO ()
main = do
  args <- getArgs
  let
    file :: String
    file = if length args /= 0
      then case head args of
             "-big" -> "bigspender.txt"
             "-f" -> args !! 1
             _ -> "normal.txt"
      else "normal.txt"

  flips :: Set.Set String <- 
    Set.fromList . lines <$> readFile file

  process (controlIO flips)

process :: (String -> IO a) -> IO ()
process k = do
  finished :: Bool <- isEOF
  unless finished $ do
    k =<< getLine
    process k

controlIO :: Set.Set String -> String -> IO ()
controlIO flips s = putStrLn $
  if (Set.member (prepIn s) flips)
    then "flip!"
    else "no"
