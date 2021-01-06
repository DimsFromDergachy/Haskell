-- Day 6: Custom Customs
-- https://adventofcode.com/2020/day/6

import Data.Char (isSpace)
import Data.List (nub)
import Data.List.Split (splitOn)

parseGroup :: String -> Int
parseGroup = length . nub . filter (not . isSpace)

partA :: String -> Int 
partA = sum . map parseGroup . splitOn "\n\n"

main :: IO()
main = getContents >>= print . partA
