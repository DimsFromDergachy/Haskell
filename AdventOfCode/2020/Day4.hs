-- Day 4: Passport Processing
-- https://adventofcode.com/2020/day/4

import Control.Monad (void)
import Data.Char (isSpace)
import Data.List (intersperse, permutations)
import Data.List.Split (splitOn)
import Text.ParserCombinators.ReadP

-- byr (Birth Year)
-- iyr (Issue Year)
-- eyr (Expiration Year)
-- hgt (Height)
-- hcl (Hair Color)
-- ecl (Eye Color)
-- pid (Passport ID)
-- cid (Country ID) - option

parserKeyValue :: String -> ReadP String
parserKeyValue key = do
    string key
    char ':'
    munch1 (not . isSpace)

byr, iyr, eyr, hgt, hcl, ecl, pid, cid :: ReadP ()
byr = void $ parserKeyValue "byr"
iyr = void $ parserKeyValue "iyr"
eyr = void $ parserKeyValue "eyr"
hgt = void $ parserKeyValue "hgt"
hcl = void $ parserKeyValue "hcl"
ecl = void $ parserKeyValue "ecl"
pid = void $ parserKeyValue "pid"
cid = optional $ parserKeyValue "cid"

-- TODO: Check the complexity (it should be less than O(n!))
shuffleAndSep :: ReadP a -> [ReadP a] -> ReadP ()
shuffleAndSep s ps = choice (map (sequence_ . intersperse s) (permutations ps))

passportParser :: ReadP ()
passportParser = shuffleAndSep skipSpaces [byr, iyr, eyr, hgt, hcl, ecl, pid, cid] >> eof

isPassport :: String -> Bool
isPassport = not . null . readP_to_S passportParser

partI :: String -> Int
partI = length . filter isPassport . splitOn "\n\n"

main :: IO()
main = getContents >>= print . partI
