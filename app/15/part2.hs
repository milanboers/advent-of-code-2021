import Data.Char (digitToInt)
import Data.Matrix as Matrix (fromLists, Matrix (nrows, ncols), safeGet, (!), matrix)
import Data.Set as Set (Set, notMember, insert, empty, fromList, filter, difference)
import Data.Maybe (isJust, fromJust)
import Data.Map as Map (Map, (!), singleton, union, findWithDefault, fromSet)
import Data.PSQueue as PSQ (PSQ, singleton, minView, Binding, key, insert)

type Pos = (Int, Int)

neighbors :: Matrix Int -> (Int, Int) -> Set (Int, Int)
neighbors matrix (r, c) = Set.filter (isJust.safeGetFromMatrix) possibleNeighbors
  where
    safeGetFromMatrix (nr, nc) = safeGet nr nc matrix
    possibleNeighbors = Set.fromList [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]

newValue :: Matrix Int -> Map Pos Int -> Int -> Pos -> Pos -> Int
newValue matrix paths maxVal curPos nbPos = min (curPath + nbVal) nbPath
  where
    curPath = findWithDefault maxVal curPos paths
    nbPath = findWithDefault maxVal nbPos paths
    nbVal = matrix Matrix.! nbPos

shortestPathsToEnd :: Matrix Int -> Int
shortestPathsToEnd matrix = shortestPathsToEnd' initialPaths empty initialUnvisited
  where
    maxVal = sum matrix
    end = (nrows matrix, ncols matrix)
    endVal = matrix Matrix.! end
    initialPaths = Map.singleton end endVal
    initialUnvisited = PSQ.singleton end endVal

    shortestPathsToEnd' :: Map Pos Int -> Set Pos -> PSQ Pos Int -> Int
    shortestPathsToEnd' paths visited unvisited
      | selected == (1,1) = paths Map.! (1,1)
      | otherwise = shortestPathsToEnd' newPaths newVisited newUnvisited
      where
        (selected', rest) = fromJust $ minView unvisited
        selected = key selected'

        unvisitedNbs = neighbors matrix selected `difference` visited

        newVisited = Set.insert selected visited

        newNbPaths = Map.fromSet (newValue matrix paths maxVal selected) unvisitedNbs
        newPaths = Map.union newNbPaths paths

        newUnvisited = foldr (\p -> PSQ.insert p (newPaths Map.! p)) rest unvisitedNbs

findAnswer :: Matrix Int -> Int
findAnswer m = spm - m Matrix.! (1,1)
  where
    newVal (r,c) = 1+((nv-1) `mod` 9)
      where
        (rd, rm) = (r-1) `divMod` nrows m
        (cd, cm) = (c-1) `divMod` ncols m
        nv = (rd+cd) + m Matrix.! (rm+1,cm+1)
    extendedM = matrix (nrows m * 5) (ncols m * 5) newVal
    spm = shortestPathsToEnd extendedM

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let grid = (map.map) digitToInt input
  print $ findAnswer . fromLists $ grid