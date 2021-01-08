-- Day 7: Handy Haversacks
-- https://adventofcode.com/2020/day/7

import Data.Char ( isLetter )
import Data.Graph ( graphFromEdges, reachable, transposeG )
import Data.Maybe ( fromJust )

type Bag = (String, String)
type Rule = (Bag, [Bag])

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = head : chunk n tail
  where
    (head, tail) = splitAt n xs

parseLine :: String -> Rule
parseLine line
-- Parse 'no other bags' option:
-- 'faded blue bags contain no other bags.'
  | length ws == 7 = ((shade, color), [])
-- 'vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.'
-- Invariant: rest always has 4x elements, second & third are a bag
  | otherwise = ((shade, color), [(sh, cs) | [_, sh, cs, _] <- chunks])
  where
    ws@(shade:color:_:_:rest) = words line
    chunks = chunk 4 rest

toKey :: Bag -> String
toKey = uncurry (++)

parseNode :: String -> ((), String, [String])
parseNode line = ((), key, keys)
  where
    (bag, bags) = parseLine line
    (key, keys) = (toKey bag, map toKey bags)

partA :: [String] -> Int
partA ss = pred $ length $ reachable (transposeG graph) vertex
  where
    (graph, _, vertexFromKey) = graphFromEdges $ map parseNode ss
    vertex = fromJust $ vertexFromKey $ toKey ("shiny", "gold")

main :: IO()
main = getContents >>= print . partA . lines
