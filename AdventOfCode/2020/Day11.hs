-- Day 11: Seating System
-- https://adventofcode.com/2020/day/11

import Control.Monad (guard, forM_)
import Data.Array (Ix, inRange)
import Data.Array (Array, (!), assocs, bounds, elems, listArray)

type Area = Array (Int, Int) Char

parse :: String -> Area
parse content = listArray bounds $ concat ls
  where
    ls = lines content
    (n,m) = (length ls, length $ head ls)
    bounds = ((1,1), (n,m))

neighborsI :: Area -> (Int, Int) -> Int
neighborsI area = length . filter (== '#') . neighbors'
  where
    neighbors' :: (Int, Int) -> [Char]
    neighbors' (i, j) = map (area !) ixs
      where
        ixs = filter (inRange (bounds area))
          [(i + di, j + dj) | di <- [-1 .. 1], dj <- [-1 .. 1], di /= 0 || dj /= 0]

neighborsII :: Area -> (Int, Int) -> Int
neighborsII area = length . filter (== '#') . neighbors'
  where
    (n, m) = snd $ bounds area
    neighbors' :: (Int, Int) -> [Char]
    neighbors' (i, j) = do
      (di, dj) <- directions
      let ixs = takeWhile (inRange (bounds area))
            $ [(i + k * di, j + k * dj) | k <- [1..]]
      let ns = filter (/= '.') $ map (area !) ixs
      guard $ not $ null ns
      return $ head ns

    directions = [(di, dj) | di <- [-1 .. 1], dj <- [-1 .. 1], di /= 0 || dj /= 0]

nextI :: Area -> Area
nextI area = listArray (bounds area) $ map next' $ assocs area
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
        ns = neighborsI area i

nextII :: Area -> Area
nextII area = listArray (bounds area) $ map next' $ assocs area
  where
    next' (i, s) = case (s, ns) of
      ('.', _) -> '.'
      ('L', 0) -> '#'
      ('L', _) -> 'L'
      ('#', 0) -> '#'
      ('#', 1) -> '#'
      ('#', 2) -> '#'
      ('#', 3) -> '#'
      ('#', 4) -> '#'
      ('#', _) -> 'L'
      where
        ns = neighborsII area i

-- TODO: Use Fix
iteration :: (Area -> Area) -> Area -> Area
iteration f a
  | f a == a = a
  | otherwise = iteration f (f a)

partI :: Area -> Int 
partI = length . filter (== '#') . elems . iteration nextI

partII :: Area -> Int
partII = length . filter (== '#') . elems . iteration nextII

-- print' :: Area -> IO ()
-- print' area = do
--   let (n, m) = snd $ bounds area
--   forM_ [1..n] $ \i -> do
--     forM_ [1..m] $ \j -> do
--       putChar $ area ! (i, j)
--     putStrLn ""

main :: IO()
main = getContents >>= print . partII . parse
