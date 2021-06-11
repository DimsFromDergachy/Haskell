import Control.Monad (guard, forM_, void)
import Data.Map as Map (Map(..), (!), fromList)
import Data.Maybe (isJust, isNothing)
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
pentaminos = fromList [
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
drawCell :: Board -> (Int, Int) -> [String]
drawCell board (i, j)
    | isNothing cell = replicate (fst size) $ replicate (snd size) ' '
    | otherwise      =
        [
            [drawNW, drawEW, drawEW, drawEW, drawEW, drawNE],
            [drawNS,  ' ',    '●',    '●',    ' ',   drawNS],
            [drawSW, drawEW, drawEW, drawEW, drawEW, drawSE]
        ]
  where
    size  = (3, 6)
    boardSize = bounds board
    cell  = board GHC.Arr.! (i, j)
    north = guard (inRange boardSize (i - 1, j)) >> board GHC.Arr.! (i - 1, j)
    east  = guard (inRange boardSize (i, j + 1)) >> board GHC.Arr.! (i, j + 1)
    south = guard (inRange boardSize (i + 1, j)) >> board GHC.Arr.! (i + 1, j)
    west  = guard (inRange boardSize (i, j - 1)) >> board GHC.Arr.! (i, j - 1)
    drawEW = '═'
    drawNS = '║'
    drawNW
      | cell == north && cell == west  =  '╬'
      | cell == north                  =  '╠'
      |                  cell == west  =  '╦'
      |            otherwise           =  '╔'
    drawNE
      | cell == north && cell == east  =  '╬'
      | cell == north                  =  '╣'
      |                  cell == east  =  '╦'
      |            otherwise           =  '╗'
    drawSW
      | cell == south && cell == west  =  '╬'
      | cell == south                  =  '╠'
      |                  cell == west  =  '╩'
      |            otherwise           =  '╚'
    drawSE
      | cell == south && cell == east  =  '╬'
      | cell == south                  =  '╣'
      |                  cell == east  =  '╩'
      |            otherwise           =  '╝'

drawBoard :: Board -> IO ()
drawBoard board = do
    forM_ [1..n] $ \i -> do
        let cells = map (\j -> drawCell board (i, j)) [1..m]
        let cells' = foldr (zipWith glueLines) [[], [], []] cells
        mapM_ putStrLn cells'
  where
    (n, m) = snd $ bounds board
    glueLines left [] = left
    glueLines left right
      | last left `elem` ['╬', '╠', '╦', '╩', '╔', '╚'] = left ++ ('═' : right)
      | otherwise = left ++ (' ' : right)

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

    drawBoard $ head xs
