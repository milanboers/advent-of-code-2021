import Data.List.Split (splitOn)
import Data.Foldable (maximumBy)

type Vel = (Int, Int)
type Pos = (Int, Int)
type Bounds = (Int, Int, Int, Int)
type Trajectory = [Pos]

inTarget :: Bounds -> Pos -> Bool
inTarget (xMin, xMax, yMin, yMax) (x,y) = xMin <= x && x <= xMax && yMin <= y && y <= yMax

overshot :: Bounds -> Pos -> Bool
overshot (_, xMax, yMin, _) (x,y) = x > xMax || y < yMin

trajectory :: Bounds -> Vel -> Trajectory
trajectory bounds@(xMin, xMax, yMin, yMax) (xV, yV) = trajectory' (0,0) (xV, yV)
  where
    trajectory' (x, y) (xV, yV)
      | inTarget bounds (x, y) = [(x,y)]
      | overshot bounds (x, y) = [(x,y)]
      | otherwise = (x, y) : trajectory' (x', y') (xV', yV')
      where
        x' = x + xV
        y' = y + yV
        xV' | xV > 0 = xV-1
            | xV < 0 = xV+1
            | otherwise = 0
        yV' = yV - 1

findAnswer :: Int -> Int -> Int -> Int -> Int
findAnswer xMin xMax yMin yMax = length trajectoriesInTarget
  where
    bounds = (xMin, xMax, yMin, yMax)
    velocities = [(x,y) | x <- [min xMin 0..max 0 xMax], y <- [yMin..abs yMin]]
    trajectories = map (trajectory bounds) velocities
    trajectoriesInTarget = filter (inTarget bounds . last) trajectories

main :: IO ()
main = do
  line <- getLine
  let [xPart, yPart] = splitOn ", " line
  let yMin = read . head . splitOn ".." $ splitOn "y=" yPart !! 1
  let yMax = read $ splitOn ".." yPart !! 1
  let xMin = read . head . splitOn ".." $ splitOn "x=" xPart !! 1
  let xMax = read $ splitOn ".." xPart !! 1
  print $ findAnswer xMin xMax yMin yMax
