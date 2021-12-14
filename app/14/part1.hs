import Data.Map as Map (fromList, Map, lookup)
import Data.List.Split (splitOn)
import Data.List (group, sort, minimumBy, maximumBy)
import Data.Function (on)

type Rules = Map (Char, Char) Char

step :: Rules -> String -> String
step rules [] = ""
step rules [x] = [x]
step rules (x:y:ys) = case Map.lookup (x,y) rules of
  Just i -> x:i:step rules (y:ys)
  Nothing -> x:step rules (y:ys)

findAnswer :: Rules -> String -> Int
findAnswer rules polymer = mostCommonCount - leastCommonCount
  where
    iterated = iterate (step rules) polymer !! 10
    grouped = group . sort $ iterated
    leastCommonCount = length . minimumBy (compare `on` length) $ grouped
    mostCommonCount = length . maximumBy (compare `on` length) $ grouped

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let (polymer:_:rules') = input
  let rules = map ((\[[a,b],[c]] -> ((a,b),c)) . splitOn " -> ") rules'
  print $ findAnswer (fromList rules) polymer
