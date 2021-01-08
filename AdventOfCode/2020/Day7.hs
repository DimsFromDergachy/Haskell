-- Day 7: Handy Haversacks
-- https://adventofcode.com/2020/day/7

import Data.Char ( isLetter )
import Data.Graph.DGraph ( DGraph, fromArcsList )
import Data.Graph.Traversal (dfsVertices)
import Data.Graph.Types ( Arc(..) )
import Data.Maybe ( fromJust )

type Bag = (String, String)
type Rule = (Bag, [(Int, Bag)])
type Graph = DGraph Bag Int

shinyGold :: Bag
shinyGold = ("shiny", "gold")

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
  | otherwise = ((shade, color), map chunkToRule $ chunk 4 rest)
  where
    ws@(shade:color:_:_:rest) = words line
    chunkToRule [count, shade, color, _] = (read count, (shade, color))

buildGraph :: [String] -> Graph
buildGraph = fromArcsList . concatMap (ruleToArcs . parseLine)
  where
    ruleToArcs :: (Bag, [(Int, Bag)]) -> [Arc Bag Int]
    ruleToArcs (bag, bags) = map (uncurry . flip $ Arc bag) bags

partA :: [String] -> Int
partA = pred . length . flip dfsVertices shinyGold . buildGraph

main :: IO()
main = getContents >>= print . partA . lines
