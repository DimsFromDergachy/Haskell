-- https://www.hackerrank.com/challenges/lambda-march-concave-polygon/problem

import Control.Monad (replicateM)
import Data.Function (on)
import Data.List (group, sortBy)
import Prelude hiding (reads)

type Point   =  (Int, Int)
type Point'  =  (Double, Double)

reads :: Read a => IO [(a, a)]
reads  =  readLn >>= flip replicateM reads''
  where
    reads'  =  getLine >>= pure . map read . words
    reads''  =  reads' >>= pure . \[x, y] -> (x, y)

middle :: [Point] -> Point'
middle  =  (\(xs, ys) -> (average' xs, average' ys)) . unzip
  where
    average' xs  =  fromIntegral (sum xs) / fromIntegral (length xs)

sort :: [Point] -> [Point]
sort ps  =  sortBy compareByAngle' ps
  where
    fromIntegral' (x, y)  =  (fromIntegral x, fromIntegral y)
    angle' (x0, y0)  =  \(x, y) -> atan2 (y - y0) (x - x0)
    compareByAngle'  =  compare `on` (angle' (middle ps) . fromIntegral')

solve :: [Point] -> Bool
solve  =  not . null . tail . group . filter (/= 0) . zipWith' cross' . zipWith' vector'
  where
    zipWith' f xs  =  zipWith f xs (tail $ cycle xs)
    vector' (x1, y1) (x2, y2)  =  (x2 - x1, y2 - y1)
    cross'  (a1, b1) (a2, b2)  =  signum $ a1 * b2 - a2 * b1

prints :: Bool -> IO ()
prints True   =  putStrLn "YES"
prints False  =  putStrLn "NO"

main :: IO()
main  =  reads >>= pure . solve . sort >>= prints
