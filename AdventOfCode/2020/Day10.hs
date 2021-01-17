-- Day 10: Adapter Array
-- https://adventofcode.com/2020/day/10

import Data.List (sort)

parseInput :: String -> [Int]
parseInput = map read . lines

zipNeighbors :: (a -> a -> b) -> [a] -> [b]
zipNeighbors f xs  =  zipWith f xs (tail $ cycle xs)

partA :: [Int] -> Int
partA xs = length (filter (== 1) ds) * length (filter (== 3) ds)
  where
    ds = zipNeighbors (-) $ sort xs

main :: IO ()
main = getContents >>= print . partA . parseInput