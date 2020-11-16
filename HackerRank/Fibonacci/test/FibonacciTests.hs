module Main (main) where

import Fibonacci (fibonacci)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options
import Test.Tasty.QuickCheck

simpleTests :: TestTree
simpleTests  =  testGroup "Fibonacci's tests"
  [
    test "Zeroth" 0 (fibonacci !! 0),
    test "First"  1 (fibonacci !! 1),
    test "Second" 1 (fibonacci !! 2),
    test "Third"  2 (fibonacci !! 3),
    test "Max by module" (102334155 `mod` (10^8 + 7)) (fibonacci !! 40)
  ]
  where
    test :: String -> Int -> Int -> TestTree
    test name exp  =  testCase name . assertEqual "" exp

allNumbersTests :: TestTree
allNumbersTests  =  testGroup "Fibonacci's test all numbers"
  $ zipWith test [0..] $
    [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55] ++
    [89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765] ++
    [10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811, 514229, 832040] ++
    [1346269, 2178309, 3524578, 5702887, 9227465, 14930352, 24157817, 39088169, 63245986]
  where
    test n exp  =  testCase (show n) $ assertEqual "" exp (fibonacci !! n)

quickCheckTests :: TestTree
quickCheckTests  =  testGroup "Test by QuickCheck"
  [
    testProperty "∀ n ≥ 0: F(n+2) == F(n+1) + F(n)" $
        \n -> n >= 0 ==> fib (n+2) == fib (n+1) + fib n,
    testProperty "∀ n ≥ 0: F(n) ≥ 0" $
        \n -> n >= 0 ==> fib n >= 0,
    testProperty "∀ n ≥ 0: F(n+1) ≥ F(n)" $
        \n -> n >= 0 ==> fib (n+1) >= fib n
  ]
  where
    fib n  =  fibonacci !! n

unitTests :: TestTree
unitTests  =  testGroup "Unit tests" [simpleTests, allNumbersTests, quickCheckTests]

main :: IO ()
main  =  defaultMain unitTests
