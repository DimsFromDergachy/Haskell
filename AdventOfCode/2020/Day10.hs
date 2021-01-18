-- Day 10: Adapter Array
-- https://adventofcode.com/2020/day/10

import Data.List (sort)

parseInput :: String -> [Int]
parseInput = map read . lines

zipNeighbors :: (a -> a -> b) -> [a] -> [b]
zipNeighbors f xs  =  zipWith f xs $ tail $ cycle xs

partA :: [Int] -> Int
partA xs = (1 + length (filter (== 1) ds)) * (1 + length (filter (== 3) ds))
  where
    ds = zipNeighbors (flip (-)) $ sort xs

-- d[x] - amount of ways to reach x-th adapter (0 if adapter doesn't exist)
-- d[x] = d[x-1] + d[x-2] + d[x-3]
partB :: [Int] -> Int
partB xs = go 0 [1, 0, 0] xs
  where
    go _ [d1, d2, d3] [] = d1
    go i [d1, d2, d3] (x:xs)
      | i == x = go (i+1) [d3 + d2 + d1, d1, d2] xs
      | otherwise = go (i+1) [0, d1, d2] (x:xs)

main :: IO ()
main = getContents >>= print . partB . sort . parseInput
