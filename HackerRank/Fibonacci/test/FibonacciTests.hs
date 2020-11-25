module Main (main) where

import Fibonacci (fibonacci, solve)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

simpleTests :: TestTree
simpleTests  =  testGroup "Test solver"
  [
    test "Zeroth" 0 (solve 0),
    test "First"  1 (solve 1),
    test "Second" 1 (solve 2),
    test "Third"  2 (solve 3),
    test "Max by module" (102334155 `mod` (10^8 + 7)) (solve 40)
  ]
  where
    test :: String -> Integer -> Integer -> TestTree
    test name exp  =  testCase name . assertEqual "" exp

allNumbersTests :: TestTree
allNumbersTests  =  testGroup "Fibonacci's test all small numbers"
  $ zipWith test [0..] $
    [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55] ++
    [89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765] ++
    [10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811, 514229, 832040] ++
    [1346269, 2178309, 3524578, 5702887, 9227465, 14930352, 24157817, 39088169, 63245986]
  where
    test n exp  =  testCase (show n) $ assertEqual "" exp (solve n)

quickCheckTests :: TestTree
quickCheckTests  =  testGroup "Test properties by QuickCheck"
  [
    testProperty "The main spec: ∀ n ≥ 0: F(n+2) = F(n) + F(n+1)" $
        \(NonNegative n) -> fib (n+2) == fib n + fib (n+1),
    testProperty "Not negative: ∀ n ≥ 0: F(n) ≥ 0" $
        \(NonNegative n) -> fib n >= 0,
    testProperty "Monotonic: ∀ n: F(n+1) ≥ F(n)" $
        \(NonNegative n) -> fib (n+1) >= fib n,
    testProperty "Cassini's identity: F(n+1) F(n−1) − F(n)^2 = (−1)^n" $
        \(Positive n) -> fib (n+1) * fib (n-1) - (fib n)^2 == (-1)^n,
    testProperty "Divisibility property: gcd(F(m), F(n)) = F(gcd(m, n))" $
        \(Positive m, Positive n) -> gcd (fib m) (fib n) == fib (gcd m n)
   ]
  where
    fib n  =  fibonacci !! n

unitTests :: TestTree
unitTests  =  testGroup "Unit tests" [simpleTests, allNumbersTests, quickCheckTests]

main :: IO ()
main  =  defaultMain unitTests
