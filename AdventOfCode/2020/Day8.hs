-- Day 8: Handheld Halting
-- https://adventofcode.com/2020/day/8

import Data.Array ( (!), Array, bounds, listArray )
import Data.Char ( isSpace )
import Data.Either ( fromLeft )
import Data.Set ( empty, insert, member )
import Text.ParserCombinators.ReadP

data Op = Nop | Acc Int | Jmp Int
  deriving Eq

type Program = Array Int Op

-- Left acc - infinity loop
-- Right acc - terminated execution
type Result = Either Int Int

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

parserOp :: ReadP Op
parserOp = choice [nop, acc, jmp]

parseOp :: String -> Op
parseOp = fst . head . readP_to_S parserOp

parseProgram :: [String] -> Program
parseProgram lines = listArray (1, length lines) $ map parseOp lines

execute :: Program -> Result
execute program = execute' 0 1 empty
  where
    (_, n) = bounds program
    execute' acc ir set
      | ir > n = Right acc -- Normal termination
      | ir `member` set = Left acc -- Infinity loop
      | otherwise = case program ! ir of
            Nop -> execute' acc (ir + 1) set'
            Acc x -> execute' (acc + x) (ir + 1) set'
            Jmp x -> execute' acc (ir + x) set'
      where
        set' = insert ir set

partA :: Program -> Int
partA = fromLeft undefined . execute

main :: IO()
main = getContents >>= print . partA . parseProgram . lines
