import Data.List.Split (splitOn)

moveCost :: Int -> Int
moveCost x = x*(x+1) `div` 2

findAnswer :: [Int] -> Int
findAnswer crabs = minimum . map totalCost $ [minCrab..maxCrab]
  where
    crabMoveCost m = moveCost . abs . (m-)
    totalCost m = sum.map (crabMoveCost m) $ crabs
    minCrab = minimum crabs
    maxCrab = maximum crabs

main :: IO ()
main = do
  line <- getLine
  let crabs = map read . splitOn "," $ line
  print . findAnswer $ crabs
