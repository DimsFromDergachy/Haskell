module Main (main) where

import ConcavePolygon (solve)
import Test.HUnit

assertIsConcave, assertIsNotConcave :: String -> Bool -> Assertion
assertIsConcave    s  =  assertBool s . not
assertIsNotConcave s  =  assertBool s

tests :: Test
tests  =  TestList
  [
    TestCase $ assertIsConcave "Triangle 1" $ tester [0,0, 0,1, 1,0],
    TestCase $ assertIsConcave "Sample Input" $ tester [0,0, 0,1, 1,1, 1,0],
    TestCase $ assertIsConcave "Sample Input #0" $ tester [622,991, 1054,665, 661,485],
    TestCase $ assertIsConcave "Sample Input #1" $ tester
      [1042,943, 793,1042, 404,909, 574,474, 1077,721, 392,543, 572,1005, 963,1020, 857,390]
  ]

testCase :: String -> Assertion -> [Int] -> Test
testCase ps = undefined
  where
    toPoints' [] = []
    toPoints' (x:y:xs) = (x,y):toPoints' xs

tester :: [Int] -> Bool
tester  =  solve . tester'
  where
    tester' []        =  []
    tester' (x:y:xs)  =  (x,y) : tester' xs

main :: IO ()
main = do
    runTestTTAndExit tests
