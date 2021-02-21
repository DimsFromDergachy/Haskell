-- Day 13: Shuttle Search
-- https://adventofcode.com/2020/day/13

import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Math.NumberTheory.Moduli.Chinese (chinese)

parse :: String -> [(Integer, Integer)]
parse = map (second $ negate . read) . filter ((/= "x") . snd) . zip [0..] . splitOn ","

partI :: Integer -> [(Integer, Integer)] -> Integer
partI start buses = (depart - start) * bus
  where
    (depart, bus) = head $ [(t, bus) | t <- [start ..], bus <- map snd buses, t `mod` bus == 0]

-- https://en.wikipedia.org/wiki/Chinese_remainder_theorem
-- x ≡ 0 (mod 3)
-- x ≡ 3 (mod 4)
-- x ≡ 4 (mod 5)
-- Solution:
-- x ≡ 39 (mod 60)
chinese' :: [(Integer, Integer)] -> (Integer, Integer)
chinese' = foldl1 f
  where
    f p1@(a1, n1) p2@(a2, n2) = (fromJust $ chinese p1 p2, n1 * n2)

partII :: Integer -> [(Integer, Integer)] -> Integer
partII start buses
  | z <= a' = start + (a' - z)
  | otherwise = start + (a' - z + n)
  where
    (a, n) = chinese' buses
    a' = a `mod` n
    z = start `mod` n
    -- a `mod` n = 39
    -- start = 1312
    -- x:
    --  x >= start
    --  x `mod` n = 39
    -------------------
    -- start `mod` n = z
    -- z <  39 ? x = start + (39 - z)
    -- z >= 39 ? x = start + (39 + n - z)
    ------------------------------------
    -- x = start + (z < 39 ? 0 : n) + 39 - z
    -- start - z = [start / n]

main :: IO ()
main = do
    start <- readLn
    buses <- getLine <&> parse
    print $ partII start buses