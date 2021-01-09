-- Day 8: Handheld Halting
-- https://adventofcode.com/2020/day/8

import Data.Array ( (!), listArray )
import Data.Char ( isSpace )
import Data.Set ( empty, insert, member )
import Text.ParserCombinators.ReadP

data Op = Nop | Acc Int | Jmp Int
  deriving Eq

nop :: ReadP Op
nop = Nop <$ string "nop"

acc :: ReadP Op
acc = do
    string "acc"
    skipSpaces
    munch (== '+')
    arg <- munch1 $ not . isSpace
    pure $ Acc $ read arg

jmp :: ReadP Op
jmp = do
    string "jmp"
    skipSpaces
    munch (== '+')
    arg <- munch1 $ not . isSpace
    pure $ Jmp $ read arg

parser :: ReadP Op
parser = choice [nop, acc, jmp]

parse :: String -> Op
parse = fst . head . readP_to_S parser

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
