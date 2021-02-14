-- Day 12: Rain Risk
-- https://adventofcode.com/2020/day/12

type Direction = (Int, Int)
type Position = (Int, Int)
type Ship = (Direction, Position)

start :: Ship
start = ((1, 0), (0, 0))

left, right :: Direction -> Direction
left  (dx, dy) = (-dy, dx)
right (dx, dy) = (dy, -dx)

move :: Ship -> String -> Ship
move ((dx, dy), (x, y)) line = case c of
    'E' -> ((dx, dy), (x + z, y))
    'S' -> ((dx, dy), (x, y - z))
    'W' -> ((dx, dy), (x - z, y))
    'N' -> ((dx, dy), (x, y + z))
    'F' -> ((dx, dy), (x + dx * z, y + dy * z))
    'L' -> (turn left (dx, dy), (x, y))
    'R' -> (turn right (dx, dy), (x, y))
  where
    (c, z) = (head line, read $ tail line)
    turn f = foldl (.) id (replicate (z `div` 90) f)

distance :: Ship -> Int
distance (_, (x, y)) = abs x + abs y

partI :: [String] -> Int 
partI = distance . foldl move start

main :: IO ()
main = getContents >>= print . partI . lines