-- Day 2: Password Philosophy
-- https://adventofcode.com/2020/day/2

-- 1-3 a: abcde
-- 1-3 b: cdefg
-- 2-9 c: ccccccccc
-- TODO: Use a parser (e.g. readP)
parse :: String -> (Int, Int, Char, String)
parse s = (read a, read b, head cs, ps)
  where
    [a, b, cs, ps] = words $ map (\c -> if c == '-' then ' ' else c) s

matchPolicyI :: (Int, Int, Char, String) -> Bool
matchPolicyI (a, b, c, p) = a <= m && m <= b
  where
    m = length $ filter (== c) p

matchPolicyII :: (Int, Int, Char, String) -> Bool
matchPolicyII (a, b, c, p) = (p !! (a-1) == c) `xor` (p !! (b-1) == c)
  where
    (xor) a b = a /= b    

solve :: [String] -> Int
solve = length . filter matchPolicyII . map parse

main :: IO ()
main = readFile "day2.input" >>= pure . lines >>= print . solve
