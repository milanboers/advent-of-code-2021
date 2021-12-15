import Data.Char (digitToInt)
import Data.Matrix as Matrix (fromLists, Matrix (nrows, ncols), safeGet, (!))
import Data.Set as Set (Set, notMember, insert, empty)
import Data.Maybe (isJust)
import Data.List (sortOn, nub)
import Data.Map as Map (Map, (!), singleton, fromList, union, findWithDefault, delete)

type Pos = (Int, Int)

neighbors :: Matrix Int -> (Int, Int) -> [(Int, Int)]
neighbors matrix (r, c) = filter (isJust.safeGetFromMatrix) possibleNeighbors
  where
    safeGetFromMatrix (nr, nc) = safeGet nr nc matrix
    possibleNeighbors = [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]

newValue :: Matrix Int -> Map Pos Int -> Int -> Pos -> Pos -> Int
newValue matrix paths maxVal curPos nbPos = min (curPath + nbVal) nbPath
  where
    curPath = findWithDefault maxVal curPos paths
    nbPath = findWithDefault maxVal nbPos paths
    nbVal = matrix Matrix.! nbPos

shortestPathsToEnd :: Matrix Int -> Int
shortestPathsToEnd matrix = shortestPathsToEnd' (Map.singleton end (matrix Matrix.! end)) Set.empty [end]
  where
    maxVal = sum matrix
    end = (nrows matrix, ncols matrix)
    shortestPathsToEnd' :: Map Pos Int -> Set Pos -> [Pos] -> Int
    shortestPathsToEnd' paths visited unvisited
      | selected == (1,1) = paths Map.! (1,1)
      | otherwise = shortestPathsToEnd' newPaths newVisited newUnvisited
      where
        (selected:rest) = sortOn (\pos -> findWithDefault maxVal pos paths) unvisited

        unvisitedNbs = filter (`notMember` visited) $ neighbors matrix selected

        newUnvisited = nub $ rest ++ unvisitedNbs
        newVisited = insert selected visited

        newNbPaths = Map.fromList $ map (\nbPos -> (nbPos, newValue matrix paths maxVal selected nbPos)) unvisitedNbs
        newPaths = Map.union newNbPaths paths


findAnswer :: Matrix Int -> Int
findAnswer m = spm - m Matrix.! (1,1)
  where
    spm = shortestPathsToEnd m

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let grid = (map.map) digitToInt input
  print $ findAnswer . fromLists $ grid