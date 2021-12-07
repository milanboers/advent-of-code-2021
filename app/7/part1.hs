import Data.List.Split (splitOn)
import Data.List (sort)

median :: [Int] -> Int
median l = sort l !! (length l `div` 2)

findAnswer :: [Int] -> Int
findAnswer crabs = sum . map (abs.(m-)) $ crabs
  where
    m = median crabs

main :: IO ()
main = do
  line <- getLine
  let crabs = map read . splitOn "," $ line
  print . findAnswer $ crabs
