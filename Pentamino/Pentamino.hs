import Control.Monad (guard, forM_, void)
import Data.Map as Map (Map(..), (!), fromList)
import Data.Maybe (isJust, fromMaybe)
import Data.Set as Set (Set(..), empty, fromList)
import GHC.Arr (Array, (!), (//), bounds, listArray, array, assocs)
import GHC.Ix (inRange)

{-
    ╔═══════╗
    ║ ..... ║
    ║ .XX.. ║
    ║ ..X.. ║  =>  [(-1, 1), (0, 1), (0, 0), (0, -1), (1, -1)]
    ║ ..XX. ║
    ║ ..... ║
    ╚═══════╝
-}

type Point = (Int, Int)
type Points = [Point]

data Phigure = Phigure {
    name :: Char,
    transforms :: [Points]
}

phigure :: Char -> Phigure
phigure ch = Phigure ch $ transforms <*> pure ps 
  where
    (ps, transforms) = pentaminos Map.! ch

type Transform = (Points -> Points)

pentaminos :: Map Char (Points, [Transform])
pentaminos = Map.fromList [
    ('F', ([(0, 1), (1, 1), (-1, 0), (0, 0), (0, -1)]
        , _8ways)),
    ('I', ([(0, 2), (0, 1), (0, 0), (0, -1), (0, -2)]
        , rotation2)),
    ('L', ([(0, 2), (0, 1), (0, 0), (0, -1), (-1, -1)]
        , _8ways)),
    ('P', ([(0, 1), (1, 1), (1, 0), (0, 0), (0, -1)]
        , _8ways)),
    ('N', ([(0, 2), (0, 1), (0, 0), (-1, 0), (-1, -1)]
        , _8ways)),
    ('T', ([(-1, 1), (0, 1), (1, 1), (0, 0), (0, -1)]
        , rotation4)),
    ('U', ([(-1, 0), (-1, -1), (0, -1), (1, -1), (1, 0)]
        , rotation4)),
    ('V', ([(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1)]
        , rotation4)),
    ('W', ([(-1, -1), (0, -1), (0, 0), (1, 0), (1, 1)]
        , rotation4)),
    ('X', ([(0, 1), (-1, 0), (0, 0), (1, 0), (0, -1)]
        , identity)),
    ('Y', ([(0, 1), (0, -2), (0, 0), (1, 0), (0, -1)]
        , _8ways)),
    ('Z', ([(-1, 1), (0, 1), (0, 0), (0, -1), (1, -1)]
        , _4ways))]
  where
    identity  = [id]
    rotation2 = [id, rotate]
    rotation4 = [id, rotate, rotate . rotate, rotate . rotate . rotate]
    mirroring = [id, mirror]
    _4ways = (.) <$> mirroring <*> rotation2
    _8ways = (.) <$> mirroring <*> rotation4

rotate :: Points -> Points
rotate = fmap $ \(x, y) -> (-y, x)

mirror :: Points -> Points
mirror = fmap $ \(x, y) -> (-x, y)

-- TODO: Move out
-------------------- DRAW --------------------
data Direction = N | E | S | W
    deriving (Eq, Ord, Enum)

offsets :: Direction -> [(Int, Int)]
offsets N = [(-1, 1), (1, 1)]
offsets E = [(1, 1), (1, -1)] ++ [(0, -1), (0, 1)]
offsets S = [(1, -1), (-1, -1)] 
offsets W = [(-1, -1), (-1, 1)] ++ [(0, -1), (0, 1)]

draw :: Points -> [String]
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

printBoard :: Board -> IO ()
printBoard board = do
    let (m, n) = snd $ bounds board
    forM_ [1..m] $ \i -> do
        putStrLn $ [fromMaybe '.' $ board GHC.Arr.! (i, j) | j <- [1..n]]

--------------------  BOARD  --------------------
type Board = Array (Int, Int) (Maybe Char)

empty :: (Int, Int) -> Board
empty (m, n) = listArray ((1, 1), (m, n)) $ repeat Nothing

full :: Board -> Bool
full = isJust . traverse void

hole :: Board -> (Int, Int)
hole = fst . head . filter ((== Nothing) . snd) . assocs

puts :: Board -> Phigure -> [Board]
puts board (Phigure ch ts)
  | full board = []
  | otherwise  = do
      ps <- ts
      (x0, y0) <- ps
      let (xh, yh) = hole board
      let ps' = map (\(x, y) -> (x - x0 + xh, y - y0 + yh)) ps
      guard $ all (inRange (bounds board)) ps'
      guard $ all ((== Nothing) . (board GHC.Arr.!)) ps'
      return $ board // [(p, Just ch) | p <- ps']

solver :: Board -> [Phigure] -> [Board]
solver board [] = do
    guard $ full board
    return board
solver board phs = do
    (ph, phs') <- taker phs
    board' <- puts board ph
    solver board' phs'

taker :: [a] -> [(a, [a])]
taker xs = taker' [] xs
  where
    taker' _ [] = []
    taker' xs (x:xs') = (x, xs ++ xs') : taker' (x:xs) xs'

main ::IO ()
main = do
    let xs = solver (Main.empty (6, 10))
           $ map phigure ['F', 'I', 'L', 'P', 'N', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']

    forM_ xs $ \x -> do
        printBoard x
        putStrLn "===================="
        putStrLn ""

    putStrLn $ concat ["There is/are ", show (length xs), " solution(s)"]