import Data.List.Split (splitOn)

onDay :: Int -> [Int] -> [Int]
onDay 0 fishes = fishes
onDay day fishes = newFishes
  where
    prevDay = onDay (day - 1) fishes
    zeros = length . filter (==0) $ prevDay

    newRest = map (\x -> x-1) . filter (/=0) $ prevDay
    newFishes = newRest ++ replicate zeros 6 ++ replicate zeros 8

findAnswer :: [Int] -> Int
findAnswer = sum . map (length . onDay 80 . return)

main :: IO ()
main = do
  line <- getLine
  let fishes = map read . splitOn "," $ line
  print . findAnswer $ fishes
