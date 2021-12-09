import Data.Matrix (fromLists, safeGet, mapPos, (!))
import Data.Foldable (toList)
import Data.Char (digitToInt)


findAnswer :: [[Int]] -> Int
findAnswer heightMap = sum riskLevels
  where
    matrix = fromLists heightMap
    safeGetFromMatrix (r, c) = safeGet r c matrix
    neighbors (r, c) = concatMap (toList.safeGetFromMatrix) [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
    smallerThanNeighbors pos = all (matrix ! pos <) $ neighbors pos

    riskLevel pos v = if smallerThanNeighbors pos then v+1 else 0
    riskLevels = mapPos riskLevel matrix


main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let heightMap = (map.map) digitToInt input
  print $ findAnswer heightMap
