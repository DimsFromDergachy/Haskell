
-- https://adventofcode.com/2019/day/1

type Mass = Int
type Fuel = Int

fuel :: Mass -> Int
fuel x = x `div` 3 - 2

partI :: [Mass] -> Int
partI = sum . map fuel

partII :: [Mass] -> Fuel
partII = sum . map (sum . takeWhile (> 0) . tail . iterate fuel)

main :: IO ()
main = getContents >>= pure . map read . words >>= print . partII
