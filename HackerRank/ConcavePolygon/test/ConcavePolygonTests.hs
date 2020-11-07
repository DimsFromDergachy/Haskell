module Main (main) where

import ConcavePolygon (solve)
import Test.HUnit

assertIsConcave, assertIsNotConcave :: String -> Bool -> Assertion
assertIsConcave    s  =  assertBool s . not
assertIsNotConcave s  =  assertBool s

tests :: Test
tests  =  TestList
  [
    testCase "Triangle" assertIsConcave [0,0, 0,1, 1,0],
    testCase "Sample Input" assertIsConcave [0,0, 0,1, 1,1, 1,0],
    testCase "Sample Input #0" assertIsConcave [622,991, 1054,665, 661,485],
    testCase "Sample Input #1" assertIsConcave
      [1042,943, 793,1042, 404,909, 574,474, 1077,721, 392,543, 572,1005, 963,1020, 857,390],
    testCase "No concave" assertIsNotConcave [0,0, 3,0, 1,1, 0,3],
    testCase "Zero cross" assertIsConcave [0,0, 2,0, 2,2, 0,2, 0,1],
    testCase "Mix vertex" assertIsConcave [0,0, 1,1, 0,1, 1,0]
  ]

testCase :: String -> (String -> Bool -> Assertion) -> [Int] -> Test
testCase message assert  =  TestCase . (assert message) . solve . toPoints'
  where
    toPoints' [] = []
    toPoints' (x:y:xs) = (x,y):toPoints' xs

main :: IO ()
main = do
    runTestTTAndExit tests
