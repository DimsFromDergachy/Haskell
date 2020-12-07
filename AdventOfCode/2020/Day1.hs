-- https://adventofcode.com/2020/day/1

import Control.Monad (guard)

partI :: [Int] -> Int
partI xs = head $ do
  x <- xs
  y <- xs
  guard $ x + y == 2020
  pure (x * y)

partII :: [Int] -> Int
partII xs = head $ do
  x <- xs
  y <- xs
  z <- xs
  guard $ x + y + z == 2020
  pure (x * y * z)

main :: IO ()
main = readFile "day1.input" >>= pure . map read . words >>= print . partII
