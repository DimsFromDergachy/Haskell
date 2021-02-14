-- Day 12: Rain Risk
-- https://adventofcode.com/2020/day/12

type Position = (Int, Int)
type WayPoint = Position
type Ship = (WayPoint, Position)

start :: Ship
start = ((10, 1), (0, 0))

left, right :: Position -> Position
left  (dx, dy) = (-dy, dx)
right (dx, dy) = (dy, -dx)

move :: Ship -> String -> Ship
move (wayPoint@(wx, wy), ship@(sx, sy)) line = case c of
    'E' -> ((wx + z, wy), ship)
    'S' -> ((wx, wy - z), ship)
    'W' -> ((wx - z, wy), ship)
    'N' -> ((wx, wy + z), ship)
    'F' -> (wayPoint, (sx + z * wx, sy + z * wy))
    'L' -> (turn left wayPoint, ship)
    'R' -> (turn right wayPoint, ship)
  where
    (c, z) = (head line, read $ tail line)
    turn f = foldl (.) id (replicate (z `div` 90) f)

distance :: Ship -> Int
distance (_, (x, y)) = abs x + abs y

partII :: [String] -> Int 
partII = distance . foldl move start

main :: IO ()
main = getContents >>= print . partII . lines