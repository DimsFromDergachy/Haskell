-- https://www.hackerrank.com/challenges/fibonacci-fp/problem

--  [0, 1, 1, 2, 3, 5 ...]
--   +
--  [1, 1, 2, 3, 5, 8 ...]
-- -------------------------
--  [1, 2, 3, 5, 8, 13 ...]

module Fibonacci where

import Control.Monad (replicateM_)

fibonacci :: [Integer]
fibonacci  =  0 : 1 : zipWith (+) fibonacci (tail fibonacci)

solve :: Int -> Integer
solve = (`mod` (10^8 + 7)) . (fibonacci !!)

main :: IO ()
main  =  readLn >>= flip replicateM_ (readLn >>= print . solve)
