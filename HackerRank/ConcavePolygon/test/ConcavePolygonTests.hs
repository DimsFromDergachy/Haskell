module Main (main) where

import ConcavePolygon (solve)
import Test.Tasty
import Test.Tasty.HUnit

assertIsConcave, assertIsNotConcave :: Bool -> Assertion
assertIsConcave     =  assertBool "Should be concave"
assertIsNotConcave  =  assertBool "Should not be concave" . not

tests :: TestTree
tests  =  testGroup "All tests"
  [
    testCase' "Triangle" assertIsNotConcave [0,0, 0,1, 1,0],
    testCase' "Triangle" assertIsNotConcave [0,0, 1,0, 0,1],
    testCase' "Sample Input" assertIsNotConcave [0,0, 0,1, 1,1, 1,0],
    testCase' "Sample Input #0" assertIsNotConcave [622,991, 1054,665, 661,485],
    testCase' "Sample Input #1" assertIsNotConcave
      [1042,943, 793,1042, 404,909, 574,474, 1077,721, 392,543, 572,1005, 963,1020, 857,390],
    testCase' "No concave" assertIsConcave [0,0, 3,0, 1,1, 0,3],
    testCase' "Zero cross #1" assertIsNotConcave [0,0, 2,0, 2,2, 0,2, 0,1],
    testCase' "Zero cross #2" assertIsNotConcave [0,0, 1,0, 2,0, 2,1, 2,2, 1,2, 0,2, 0,1],
    testCase' "Zero cross no concave" assertIsConcave [0,0, 3,0, 1,1, 0,3, 0,2],
    testCase' "Mix vertex" assertIsNotConcave [0,0, 1,1, 0,1, 1,0]
  ]

testCase' :: String -> (Bool -> Assertion) -> [Int] -> TestTree
testCase' message assert  =  testCase message . assert . solve . toPoints'
  where
    toPoints' [] = []
    toPoints' (x:y:xs) = (x,y):toPoints' xs

main :: IO ()
main  =  defaultMain tests
