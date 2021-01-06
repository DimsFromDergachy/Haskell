-- Day 5: Binary Boarding
-- https://adventofcode.com/2020/day/5

import Data.Bifunctor (bimap)
import Data.Bits

fromBits :: (Char -> Bool) -> String -> Int
fromBits isBit = foldl (\r c -> shift r 1 .|. fromEnum (isBit c)) zeroBits

-- FBFBBFFRLR
parseSeat :: String -> (Int, Int)
parseSeat = bimap (fromBits (== 'B')) (fromBits (== 'R')) . splitAt 7

partA :: [String] -> Int
partA = maximum . map ((\(a, b) -> a*8 + b) . parseSeat)

main :: IO()
main = getContents >>= print . partA . lines
