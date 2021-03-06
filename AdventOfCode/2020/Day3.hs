-- Day 3: Toboggan Trajectory
-- https://adventofcode.com/2020/day/3

solve :: (Int, Int) -> [String] -> Integer
solve (x, y) = fromIntegral . length . filter (== '#') . zipWith (\n s -> (cycle s) !! n) [0, y..] . filterBy x
  where
    filterBy n = map snd . filter ((== 0) . (`mod` n) . fst) . zip [0..]

partI :: [String] -> Integer
partI = solve (1, 3)

partII :: [String] -> Integer
partII ls = product $ map (flip solve ls) [(1,1), (1,3), (1,5), (1,7), (2,1)]

main :: IO ()
main = readFile "day3.input" >>= pure . partII . lines >>= print
