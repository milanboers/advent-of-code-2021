{-# LANGUAGE TemplateHaskell #-}
import Data.List.Split (splitOn)
import Data.MultiMap as MultiMap (MultiMap, fromList, (!))
import Data.Set as Set (Set, empty, insert, notMember, member, toList, fromList)
import Data.Char (isUpper)
import Data.Tuple (swap)
import Data.Function.Memoize (deriveMemoizable, memoFix3)

type Node = String
type Path = [Node]
type Edge = (Node, Node)

deriveMemoizable ''Set

paths :: MultiMap Node Node -> Int
paths edges = memoFix3 paths' empty False "start"
  where
    paths' :: (Set Node -> Bool -> Node -> Int) -> Set Node -> Bool -> Node -> Int
    paths' f smallSeen smallVisitedTwice x@"end" = 1
    paths' f smallSeen smallVisitedTwice current = nextPaths
      where
        isSmall = not . isUpper . head $ current
        newSmallVisitedTwice = smallVisitedTwice || isSmall && current `member` smallSeen
        newSmallSeen = if isSmall then insert current smallSeen else smallSeen
        canVisit next = (not newSmallVisitedTwice || next `notMember` smallSeen) && next /= "start"

        nexts = filter canVisit $ edges ! current
        nextPaths = sum $ map (f newSmallSeen newSmallVisitedTwice) nexts

findAnswer :: [Edge] -> Int
findAnswer edges = paths edgesMap
  where
    edgesMap = MultiMap.fromList (edges ++ map swap edges)

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let splitLines = map (splitOn "-") input
  let edges = map (\[x, y] -> (x, y)) splitLines
  print $ findAnswer edges
