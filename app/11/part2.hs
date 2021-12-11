import Data.Matrix (fromLists, safeGet, Matrix (nrows, ncols), (!), setElem, mapPos)
import Data.Char (digitToInt)
import Data.Maybe (isJust, fromJust)
import Data.List (nub, findIndex)

neighbors :: Matrix Int -> (Int, Int) -> [(Int, Int)]
neighbors matrix (r, c) = filter (isJust.safeGetFromMatrix) possibleNeighbors
  where
    safeGetFromMatrix (nr, nc) = safeGet nr nc matrix
    possibleNeighbors = [(nr, nc) | nr <- [r-1..r+1], nc <- [c-1..c+1], (nr, nc) /= (r, c)]

flash :: Matrix Int -> [(Int, Int)] -> Matrix Int
flash matrix [] = matrix
flash matrix flashing = flash newMatrix newFlashing
  where
    neighborsOfFlashing = filter ((/=0).(matrix!)).concatMap (neighbors matrix) $ flashing
    newMatrix' = foldr (\pos m -> setElem ((m ! pos) + 1) pos m) matrix neighborsOfFlashing
    newMatrix = foldr (setElem 0) newMatrix' flashing
    newFlashing = filter ((>9).(newMatrix!)) (nub neighborsOfFlashing)

doStep :: Matrix Int -> Matrix Int
doStep matrix = newMatrix
  where
    newMatrix' = (+1) <$> matrix
    flashing = [(r, c) | r <- [1..nrows matrix], c <- [1..ncols matrix], newMatrix' ! (r, c) > 9]
    newMatrix = flash newMatrix' flashing

doSteps :: Matrix Int -> [Matrix Int]
doSteps = iterate doStep

findAnswer :: Matrix Int -> Int
findAnswer = fromJust . findIndex (all (==0)) . doSteps

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let grid = (map.map) digitToInt input
  print $ findAnswer . fromLists $ grid
