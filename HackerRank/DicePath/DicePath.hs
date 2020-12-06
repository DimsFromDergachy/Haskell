-- https://www.hackerrank.com/challenges/dice-path/problem

-- ⚀ ⚁ ⚂ ⚃ ⚄ ⚅

module DicePath where

import Control.Monad (replicateM_)
import Prelude hiding (Right)
import Data.Functor ((<&>))

-- Top, Front, Left, Right, Back, Down
data Face = T | F | L | R | B | D
  deriving Show

--data Pip  =  Pip Int
type Pip  =  Int

-- Faces don't matter because they're constant
-- here for pretty printing
type Dice  =  [(Pip, Face)]

data Roll  =  Right | Down

start :: Dice
start  =  [(1,T), (2,F), (3,L), (4,R), (5,B), (6,D)]

roll :: Roll -> Dice -> Dice
roll dir [t, f, l, r, b, d]  =  roll' $ rotate' dir
  where
    roll'  =  zipWith (\f f' -> (fst f', snd f)) [t, f, l, r, b, d]
    rotate' Right  =  [l, f, d, t, b, r]
    rotate' Down   =  [b, t, l, r, d, f]

-- invariant - first item is Top
score :: Dice -> Int
score  =  fst . head

solve :: Int -> Int -> Int
solve m n  =  fst $ a m n
  where
    a 1 1  =  (1, start)
    a i j
      | i == 1           =  l'
      | j == 1           =  u'
      | fst l' > fst u'  =  l'
      | otherwise        =  u'
      where
        (al, pl)  =  a i (j-1)
        l'  =  (al + score pl', pl')
          where
            pl'  =  roll Right pl
        (au, pu)  =  a (i-1) j
        u'  =  (au + score pu', pu')
          where
            pu'  =  roll Down pu

main :: IO ()
main  =  readLn >>= flip replicateM_ (reads' >>= print . uncurry solve)
  where
    reads'  =  getLine <&> (\[x, y] -> (x, y)) . map read . words
