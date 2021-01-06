-- Day 6: Custom Customs
-- https://adventofcode.com/2020/day/6

import Data.Char (isSpace)
import Data.List (nub)
import Data.List.Split (splitOn)

solveA :: String -> Int
solveA = length . nub . filter (not . isSpace)

partA :: String -> Int 
partA = sum . map solveA . splitOn "\n\n"

solveB :: [String] -> Int
solveB ss = length [() | c <- ['a' .. 'z'], all (c `elem`) ss]

partB :: String -> Int
partB = sum . map (solveB . lines) . splitOn "\n\n"

main :: IO()
main = getContents >>= print . partB
