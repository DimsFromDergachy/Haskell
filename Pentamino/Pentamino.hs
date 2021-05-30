
import Control.Monad (guard)
import Data.List (intersperse)
import Data.Map as Map (Map(..), (!), fromList)
import Data.Set as Set (Set(..), empty, fromList)

{-
    ╔═══════╗
    ║ ..... ║
    ║ .XX.. ║
    ║ ..X.. ║  =>  [(-1, 1), (0, 1), (0, 0), (0, -1), (1, -1)]
    ║ ..XX. ║
    ║ ..... ║
    ╚═══════╝
-}

f, i, l, p, n, t, u, v, w, x, y, z :: Phigure
f = [(0, 1), (1, 1), (-1, 0), (0, 0), (0, -1)]
i = [(0, 2), (0, 1), (0, 0), (0, -1), (0, -2)]
l = [(0, 2), (0, 1), (0, 0), (0, -1), (-1, -1)]
p = [(0, 1), (1, 1), (1, 0), (0, 0), (0, -1)]
n = [(0, 2), (0, 1), (0, 0), (-1, 0), (-1, -1)]
t = [(-1, 1), (0, 1), (1, 1), (0, 0), (0, -1)]
u = [(-1, 0), (-1, -1), (0, -1), (1, -1), (1, 0)]
v = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1)]
w = [(-1, -1), (0, -1), (0, 0), (1, 0), (1, 1)]
x = [(0, 1), (-1, 0), (0, 0), (1, 0), (0, -1)]
y = [(0, 1), (0, -2), (0, 0), (1, 0), (0, -1)]
z = [(-1, 1), (0, 1), (0, 0), (0, -1), (1, -1)]

data Direction = N | E | S | W
    deriving (Eq, Ord, Enum)

type Point = (Int, Int)
type Phigure = [Point]

offsets :: Direction -> [(Int, Int)]
offsets N = [(-1, 1), (1, 1)]
offsets E = [(1, 1), (1, -1)] ++ [(0, -1), (0, 1)]
offsets S = [(1, -1), (-1, -1)] 
offsets W = [(-1, -1), (-1, 1)] ++ [(0, -1), (0, 1)]

draw :: Phigure -> [String]
draw ps = do
    j <- reverse [-5 .. 5]
    guard $ odd j
    return $ do
        i <- [-5 .. 5]
        let set = Set.fromList $ do 
                d <- [N .. W]
                (dx, dy) <- offsets d
                guard $ (i + dx, j + dy) `elem` ps'
                return d
        return $ (Map.!) drawMap set
    where
      ps' = fmap (\(x, y) -> (x*2, y*2)) ps


drawMap :: Map (Set Direction) Char
drawMap = Map.fromList [
        (Set.empty,                 ' '),
        (Set.fromList [N, S],       '║'),
        (Set.fromList [E, W],       '═'),
        (Set.fromList [E, S],       '╔'),
        (Set.fromList [S, W],       '╗'),
        (Set.fromList [N, E],       '╚'),
        (Set.fromList [N, W],       '╝'),
        (Set.fromList [E, S, W],    '╦'),
        (Set.fromList [N, S, W],    '╣'),
        (Set.fromList [N, E, W],    '╩'),
        (Set.fromList [N, E, S],    '╠'),
        (Set.fromList [N, E, S, W], '╬')
    ]

rotate :: Point -> Point
rotate (x, y) = (-y, x)

main ::IO ()
main = do
    mapM_ putStrLn $ draw $ fmap rotate z
    --mapM_ (mapM_ putStrLn . draw) [f, i, l, p, n, t, u, v, w, x, y, z]