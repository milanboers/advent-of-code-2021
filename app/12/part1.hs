import Data.List.Split (splitOn)
import Data.MultiMap (MultiMap, fromList, (!))
import Data.Set (Set, empty, insert, notMember)
import Data.Char (isUpper)
import Data.Tuple (swap)

type Node = String
type Path = [Node]

paths :: MultiMap Node Node -> [Path]
paths edges = paths' empty "start"
  where
    paths' :: Set Node -> Node -> [Path]
    paths' smallSeen x@"end" = [[x]]
    paths' smallSeen current = nextPaths
      where
        newSmallSeen = if isUpper . head $ current then smallSeen else insert current smallSeen

        nexts = filter (`notMember` smallSeen) $ edges ! current
        nextPaths = [current:path | next <- nexts, path <- paths' newSmallSeen next]

findAnswer :: [(Node, Node)] -> Int
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
