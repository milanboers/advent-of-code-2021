import Data.List.Split (splitOn)
import Data.MultiMap (MultiMap, fromList, (!))
import Data.Set (Set, empty, insert, notMember, member)
import Data.Char (isUpper)
import Data.Tuple (swap)

type Node = String
type Path = [Node]
type Edge = (Node, Node)

paths :: MultiMap Node Node -> [Path]
paths edges = paths' empty False "start"
  where
    paths' :: Set Node -> Bool -> Node -> [Path]
    paths' smallSeen smallVisitedTwice x@"end" = [[x]]
    paths' smallSeen smallVisitedTwice current = nextPaths
      where
        isSmall = not . isUpper . head $ current
        newSmallVisitedTwice = smallVisitedTwice || isSmall && current `member` smallSeen
        newSmallSeen = if isSmall then insert current smallSeen else smallSeen
        canVisit next = (not newSmallVisitedTwice || next `notMember` smallSeen) && next /= "start"

        nexts = filter canVisit $ edges ! current
        nextPaths = [current:path | next <- nexts, path <- paths' newSmallSeen newSmallVisitedTwice next]

findAnswer :: [Edge] -> Int
findAnswer edges = length . paths $ edgesMap
  where
    edgesMap = fromList (edges ++ map swap edges)

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let splitLines = map (splitOn "-") input
  let edges = map (\[x, y] -> (x, y)) splitLines
  print $ findAnswer edges
