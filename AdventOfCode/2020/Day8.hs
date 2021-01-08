-- Day 8: Handheld Halting
-- https://adventofcode.com/2020/day/8

import Data.Array ( (!), listArray )
import Data.Set ( empty, insert, member )

data Op = Nop | Acc Int | Jmp Int
  deriving Eq

parse :: String -> Op
parse line
  | op == "nop" = Nop
  | op == "acc" = Acc $ read arg
  | op == "jmp" = Jmp $ read arg
  where
    [op, arg] = words line

partA :: [String] -> Int 
partA lines = execute 0 1 empty
  where
    program = listArray (1, length lines) $ map parse lines
    execute acc ir set
      | ir `member` set = acc
      | otherwise = case program ! ir of
            Nop -> execute acc (ir + 1) set'
            Acc x -> execute (acc + x) (ir + 1) set'
            Jmp x -> execute acc (ir + x) set'
      where
        set' = insert ir set

main :: IO()
main = getContents >>= print . partA . lines
