-- Day 4: Passport Processing
-- https://adventofcode.com/2020/day/4

import Control.Monad (guard, void)
import Data.Char (isHexDigit, isDigit, isSpace)
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

byr', iyr', eyr', hgt', hcl', ecl', pid', cid' :: ReadP ()
byr' = do
    string "byr"
    char ':'
    year <- read <$> munch1 isDigit
    guard $ 1920 <= year && year <= 2002
    pure ()
iyr' = do
    string "iyr"
    char ':'
    year <- read <$> munch1 isDigit
    guard $ 2010 <= year && year <= 2020
    pure ()
eyr' = do
    string "eyr"
    char ':'
    year <- read <$> munch1 isDigit
    guard $ 2020 <= year && year <= 2030
    pure ()
hgt' = do
    string "hgt"
    char ':'
    hgt <- read <$> munch1 isDigit
    unit <- string "in" +++ string "cm"
    guard $ unit == "cm" && 150 <= hgt && hgt <= 193
         || unit == "in" && 59 <= hgt && hgt <= 76
    pure ()
hcl' = do
    string "hcl"
    char ':'
    char '#'
    count 6 $ satisfy isHexDigit
    pure ()
ecl' = do
    string "ecl"
    char ':'
    choice $ map string ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    pure ()
pid' = do
    string "pid"
    char ':'
    count 9 $ satisfy isDigit
    pure ()
cid' = optional $ parserKeyValue "cid"

-- TODO: Check the complexity (it should be less than O(n!))
shuffleAndSep :: ReadP a -> [ReadP a] -> ReadP ()
shuffleAndSep s ps = choice (map (sequence_ . intersperse s) (permutations ps))

passportParserA :: ReadP ()
passportParserA = shuffleAndSep skipSpaces [byr, iyr, eyr, hgt, hcl, ecl, pid, cid] >> eof

passportParserB :: ReadP ()
passportParserB = shuffleAndSep skipSpaces [byr', iyr', eyr', hgt', hcl', ecl', pid', cid'] >> eof

isParse :: ReadP () -> String -> Bool
isParse p = not . null . readP_to_S p

partA :: String -> Int
partA = length . filter (isParse passportParserA) . splitOn "\n\n"

partB :: String -> Int
partB = length . filter (isParse passportParserB) . splitOn "\n\n"

main :: IO()
main = getContents >>= print . partB
