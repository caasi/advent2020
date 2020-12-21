module Main where

import System.Environment
import Day1
import Day2
import Day3

main :: IO ()
main = do
  args <- getArgs
  str <- getContents
  case head args of
    "1a" ->
      print $ day1a ns
      where ns = map read (lines str)
    "1b" ->
      print $ day1b ns
      where ns = map read (lines str)
    "2a" ->
      print $ day2a ns
      where ns = map read (lines str)
    "2b" ->
      print $ day2b ns
      where ns = map read (lines str)
    "3a" ->
      print $ day3a str
    _ -> print "program not found"
