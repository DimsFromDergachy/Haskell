-- Day 7: Handy Haversacks
-- https://adventofcode.com/2020/day/7

import Data.Char ( isLetter )
import Data.Graph.DGraph ( DGraph, fromArcsList, outboundingArcs, transpose )
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

parseRule :: String -> Rule
parseRule line
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
buildGraph = fromArcsList . concatMap (ruleToArcs . parseRule)
  where
    ruleToArcs :: (Bag, [(Int, Bag)]) -> [Arc Bag Int]
    ruleToArcs (bag, bags) = map (uncurry . flip $ Arc bag) bags

-- TODO: Use folding
partA :: [String] -> Int
partA = pred . length . flip dfsVertices shinyGold . transpose . buildGraph

-- shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
-- dark olive bags contain 3 faded blue bags, 4 dotted black bags.
-- vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
-- faded blue bags contain no other bags.
-- dotted black bags contain no other bags.

-- go sg = 1 + 1 * go do + 2 * go vp
-- go do = 1 + 3 * go fb + 4 * go db
-- go vp = 1 + 5 * go fb + 6 * go db
-- go fb = 1
-- go db = 1
partB :: [String] -> Int
partB lines = pred $ go shinyGold
  where
    graph = buildGraph lines
    go = (1+) . sum . map (\(Arc _ v e) -> e * go v) . outboundingArcs graph
    -- TODO: Use folding

main :: IO()
main = getContents >>= print . partB . lines
