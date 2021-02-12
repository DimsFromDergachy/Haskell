-- Day 11: Seating System
-- https://adventofcode.com/2020/day/11

import Control.Monad (forM_)
import Data.Array (Ix, inRange)
import Data.Array (Array, (!), assocs, bounds, elems, listArray)

type Area = Array (Int, Int) Char

parse :: String -> Area
parse content = listArray bounds $ concat ls
  where
    ls = lines content
    (n,m) = (length ls, length $ head ls)
    bounds = ((1,1), (n,m))

neighbors :: Area -> (Int, Int) -> Int
neighbors area = length . filter (== '#') . neighbors' area
  where
    neighbors' :: Area -> (Int, Int) -> [Char]
    neighbors' array (i, j) = map (array !) ixs
      where
        ixs = filter (inRange (bounds array))
          [(i + di, j + dj) | di <- [-1 .. 1], dj <- [-1 .. 1], di /= 0 || dj /= 0]

next :: Area -> Area
next area = listArray (bounds area) $ map next' $ assocs area
  where
    next' (i, s) = case (s, ns) of
      ('.', _) -> '.'
      ('L', 0) -> '#'
      ('L', _) -> 'L'
      ('#', 0) -> '#'
      ('#', 1) -> '#'
      ('#', 2) -> '#'
      ('#', 3) -> '#'
      ('#', _) -> 'L'
      where
        ns = neighbors area i

-- TODO: Use Fix
iteration :: Area -> Area
iteration a
  | next a == a = a
  | otherwise = iteration (next a)

partI :: Area -> Int 
partI = length . filter (== '#') . elems . iteration

-- print' :: Area -> IO ()
-- print' area = do
--   let (n, m) = snd $ bounds area
--   forM_ [1..n] $ \i -> do
--     forM_ [1..m] $ \j -> do
--       putChar $ area ! (i, j)
--     putStrLn ""

main :: IO()
main = getContents >>= print . partI . parse
