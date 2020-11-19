{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import ConcavePolygon (solve)

import Control.Monad (guard)
import Data.List (nub)
import Data.Maybe (isJust)
import Test.Tasty
import Test.Tasty.QuickCheck

type Point = (Int, Int)
newtype Triangle = Tr { getPoints :: [Point] }
  deriving (Show, Eq)

instance Arbitrary Triangle
  where
    arbitrary = Tr <$> suchThat (vectorOf 3 arbitrary) isTriangle
      where
        isTriangle [p1@(x1,y1), p2@(x2,y2), p3@(x3,y3)] = isJust $ do
            guard $ p1 /= p2
            guard $ p1 /= p3
            guard $ p2 /= p3
            guard $ (x2 - x1) * (y3 - y1) /= (x3 - x1) * (y2 - y1)
            pure ()

triangleTest :: TestTree
triangleTest = testProperty "Triangle is always not a concave polygon"
    $ \(tr :: Triangle) -> not . isConcave . solve . getPoints
  where
    isConcave = id

tests :: TestTree
tests = testGroup "Test by QuickCheck:"
  [
    triangleTest
  ]

main :: IO ()
main = defaultMain tests
