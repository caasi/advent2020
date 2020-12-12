module Lib
    ( day1a
    , day1b
    ) where

day1a :: [Int] -> Int
day1a ns =
  let xs = do
        x <- ns
        y <- ns
        if x + y == 2020
          then return (x * y)
          else []
  in head xs

day1b :: [Int] -> Int
day1b ns =
  let xs = do
        x <- ns
        y <- ns
        z <- ns
        if x + y + z == 2020
          then return (x * y * z)
        else []
  in head xs

