-- Day 2: Password Philosophy
-- https://adventofcode.com/2020/day/2

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

type Policy = (Int, Int, Char, String)

getNumber :: ReadP Int
getNumber = pure read <*> munch1 isDigit

getRest :: ReadP String
getRest = manyTill get eof

-- 1-3 a: abcde
-- 1-3 b: cdefg
-- 2-9 c: ccccccccc
policyParser :: ReadP Policy
policyParser = pure (,,,)
    <*> getNumber
    <*  char '-'
    <*> getNumber
    <*  skipSpaces
    <*> get
    <*  char ':'
    <*  skipSpaces
    <*> getRest

parse :: String -> Policy
parse = fst . head . readP_to_S policyParser

matchPolicyI :: Policy -> Bool
matchPolicyI (a, b, c, p) = a <= m && m <= b
  where
    m = length $ filter (== c) p

matchPolicyII :: Policy -> Bool
matchPolicyII (a, b, c, p) = (p !! (a-1) == c) `xor` (p !! (b-1) == c)
  where
    (xor) a b = a /= b

solve :: [String] -> Int
solve = length . filter (matchPolicyII . parse)

main :: IO ()
main = readFile "day2.input" >>= pure . lines >>= print . solve
