-- Day 10: Adapter Array
-- https://adventofcode.com/2020/day/10

import Data.Array ((!), accumArray)
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
partB xs = d ! n
  where
    d = accumArray (flip const) 0 (-2, n) $
      (0, 1) : [(x, d ! (x-3) + d ! (x-2) + d ! (x-1)) | x <- xs]
    n = maximum xs

main :: IO ()
main = getContents >>= print . partB . parseInput
