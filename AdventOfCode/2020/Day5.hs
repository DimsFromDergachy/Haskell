-- Day 5: Binary Boarding
-- https://adventofcode.com/2020/day/5

import Data.Bifunctor (bimap)
import Data.Bits ((.|.), shift, zeroBits)
import Data.List (sort)

fromBits :: (Char -> Bool) -> String -> Int
fromBits isBit = foldl (\r c -> shift r 1 .|. fromEnum (isBit c)) zeroBits

-- FBFBBFFRLR
parseSeat :: String -> (Int, Int)
parseSeat = bimap (fromBits (== 'B')) (fromBits (== 'R')) . splitAt 7

parseId :: String -> Int
parseId = (\(a, b) -> a*8 + b) . parseSeat

partA :: [String] -> Int
partA = maximum . map ((\(a, b) -> a*8 + b) . parseSeat)

findHole :: [Int] -> Int
findHole = (+1) . fst . head . dropWhile (\(a, b) -> a + 1 == b) . pair . sort
  where
    pair xs = zip xs $ tail xs

partB :: [String] -> Int
partB = findHole . map parseId

main :: IO()
main = getContents >>= print . partB . lines
