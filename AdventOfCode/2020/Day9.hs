-- Day 9: Encoding Error
-- https://adventofcode.com/2020/day/9

import Control.Monad ( guard )
import Data.Array ( Array, (!), bounds, listArray )

parseInput :: String -> Array Int Int
parseInput input = listArray (1, length strings) $ map read strings
  where
    strings = lines input

partA :: Array Int Int -> Int
partA a = head $ do
    l <- [1 .. n - 25]
    let r = l + 25
    guard $ null $ do
        i <- [l .. r - 1]
        j <- [l .. r - 1]
        guard $ (a ! i) /= (a ! j)
        guard $ (a ! i) + (a ! j) == (a ! r)
    pure $ a ! r
  where
    (_, n) = bounds a

partB :: Array Int Int -> Int -> Int
partB a x = go 1 0 0
  where
    go l r sum
      | sum > x = go (l + 1) r (sum - a ! l)
      | sum < x = go l (r + 1) (sum + a ! (r + 1))
      | otherwise = minimum as + maximum as
      where
        as = [a ! i | i <- [l .. r]]

main :: IO ()
main = do
  array <- fmap parseInput getContents
  print $ partB array $ partA array
