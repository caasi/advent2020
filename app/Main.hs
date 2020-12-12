module Main where

import Lib

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
  str <- getContents
  let ns = map readInt (lines str)
  print $ [day1a ns, day1b ns]
