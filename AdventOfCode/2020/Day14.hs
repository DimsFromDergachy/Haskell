-- Day 14: Docking Data
-- https://adventofcode.com/2020/day/14

import Data.Bits (Bits, clearBit, setBit)
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.IntMap (IntMap, elems, empty, insert)
import Text.ParserCombinators.ReadP

type Int36 = Int64
data Command = Mask String | SetMemory Int Int36

commandParser :: ReadP Command
commandParser = maskP +++ setMemoryP
  where
    maskP = Mask 
      <$  string "mask = "
      <*> count 36 get
    setMemoryP = SetMemory
      <$  string "mem"
      <*  char '['
      <*> (read <$> munch isDigit)
      <*  char ']'
      <*  string " = "
      <*> (read <$> munch isDigit)

parse :: String -> Command
parse = fst . head . readP_to_S commandParser

-- value:  000000000000000000000000000000001011  (decimal 11)
-- mask:   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
-- result: 000000000000000000000000000001001001  (decimal 73)
applyMask :: Bits a => String -> a -> a
applyMask mask = flip (foldl setBit') mask'
  where
    mask' = filter ((/= 'X') . snd) $ zip [0..] $ reverse mask
    setBit' a (x, '0') = clearBit a x
    setBit' a (x, '1') = setBit a x

partI :: [Command] -> Int36
partI = sum . elems . snd . execute
  where
    execute = foldl execute' (undefined, empty)
    execute' (_, memory) (Mask mask) = (mask, memory)
    execute' (mask, memory) (SetMemory a v) = (mask, insert a (applyMask mask v) memory)

main :: IO ()
main = getContents >>= print . partI . map parse . lines