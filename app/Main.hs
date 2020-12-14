module Main where

import Lib

readInt :: String -> Int
readInt = read

readRange :: String -> Range
readRange = read

readPassword :: String -> Password
readPassword = read

main :: IO ()
main = do
  str <- getContents
  let ns = map readPassword (lines str)
  print $ [day2a ns, day2b ns]
