module Main (main) where

import ConcavePolygon (solve)
import Test.Tasty
import Test.Tasty.QuickCheck

-- Phigures --

type Point = (Int, Int)
type Points = [Point]

newtype Polygon = Polygon Points
  deriving (Show, Eq)
data Triangle = Triangle Point Point Point
  deriving (Show, Eq)

instance Arbitrary Triangle
  where
    arbitrary = flip suchThat isTriangle
        $ Triangle <$> arbitrary <*> arbitrary <*> arbitrary
      where
        -- ABC is triangle ⇔ AB · BC != 0
        isTriangle (Triangle (x1,y1) (x2,y2) (x3,y3)) = (x2 - x1) * (y3 - y1) /= (x3 - x1) * (y2 - y1)

instance Arbitrary Polygon
  where
    arbitrary = do
      (Triangle p1 p2 p3) <- arbitrary :: Gen Triangle
      ps <- listOf arbitrary :: Gen Points
      Polygon <$> shuffle (p1 : p2 : p3 : ps)

data ShuffleVertexes = ShuffleVertexes Points Points
  deriving (Show, Eq)

instance Arbitrary ShuffleVertexes
  where
    arbitrary = do
      (Polygon ps) <- arbitrary
      ps' <- shuffle ps
      pure $ ShuffleVertexes ps ps'

-- Tests --

triangleTest :: TestTree
triangleTest = testProperty "Triangle is always not a concave polygon"
    $ \(Triangle p1 p2 p3) -> isNotConcave $ solve $ [p1, p2, p3]
  where
    isNotConcave = not

polygonTest :: TestTree
polygonTest = testProperty "Classify random polygons (no test)"
    $ \(Polygon ps) -> classify (solve ps) "is concave" $ True

shuffleVertexes :: TestTree
shuffleVertexes = testProperty "Shuffle vertexes doesn't change the type"
    $ \(ShuffleVertexes p1 p2) -> solve p1 == solve p2

tests :: TestTree
tests = testGroup "Test by QuickCheck:"
  [
    triangleTest,
    polygonTest,
    shuffleVertexes
  ]

main :: IO ()
main = defaultMain tests
