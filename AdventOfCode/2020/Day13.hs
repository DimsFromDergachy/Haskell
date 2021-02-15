-- Day 13: Shuttle Search
-- https://adventofcode.com/2020/day/13

import Data.Functor ((<&>))
import Data.List.Split (splitOn)

parse :: String -> [Int]
parse = map read . filter (/= "x") . splitOn ","

partI :: Int -> [Int] -> Int
partI start buses = (depart - start) * bus
  where
    (depart, bus) = head $ [(t, bus) | t <- [start ..], bus <- buses, t `mod` bus == 0]

main :: IO ()
main = do
    start <- readLn
    buses <- getLine <&> parse
    print $ partI start buses