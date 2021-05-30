
import Control.Monad (guard)
import Data.List (intersperse)
import Data.Map as Map (Map(..), (!), keys, fromList)
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

data Direction = N | E | S | W
    deriving (Eq, Ord, Enum)

type Point = (Int, Int)
type Points = [Point]
--type Phigure = [Points] -- a list of possible transformations: rotates/mirrors

offsets :: Direction -> [(Int, Int)]
offsets N = [(-1, 1), (1, 1)]
offsets E = [(1, 1), (1, -1)] ++ [(0, -1), (0, 1)]
offsets S = [(1, -1), (-1, -1)] 
offsets W = [(-1, -1), (-1, 1)] ++ [(0, -1), (0, 1)]

-- TODO: Move out
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

rotate :: Points -> Points
rotate = fmap $ \(x, y) -> (-y, x)

mirror :: Points -> Points
mirror = fmap $ \(x, y) -> (-x, y)

main ::IO ()
main = do
    mapM_ (\ch -> do
        putStrLn $ concat [replicate 10 '=', " ", [ch], " ", replicate 10 '=']
        mapM_ (mapM_ putStrLn . draw) $ transforms $ phigure ch)
        $ Map.keys pentaminos