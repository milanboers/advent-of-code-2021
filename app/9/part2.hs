import Data.Matrix (fromLists, safeGet, (!), Matrix (ncols, nrows))
import Data.List (sort, nub)
import Data.Maybe (isJust)
import Data.Char (digitToInt)


findAnswer :: [[Int]] -> Int
findAnswer heightMap = product . take 3 . reverse . sort . map (length.basinPoss) $ lowPoints
  where
    matrix = fromLists heightMap
    safeGetFromMatrix (r, c) = safeGet r c matrix
    neighborPoss (r, c) = filter (isJust.safeGetFromMatrix) [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

    neighbors = map (matrix !) . neighborPoss
    smallerThanNeighbors pos = all (matrix ! pos <) $ neighbors pos
    lowPoints = [(r, c) | c <- [1..ncols matrix], r <- [1..nrows matrix], smallerThanNeighbors (r, c)]

    basinPoss pos = if v == 9 then [] else nub $ pos : concatMap basinPoss neighborBasinCoords
      where
        v = matrix ! pos
        neighborBasinCoords = filter ((>v).(matrix !)) . neighborPoss $ pos


main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let heightMap = (map.map) digitToInt input
  print $ findAnswer heightMap
