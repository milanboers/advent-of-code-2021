import Data.List.Split (splitOn)

onDay :: Int -> [(Int,Int)] -> [(Int,Int)]
onDay 0 fishes = fishes
onDay day fishes = newFishes
  where
    prevDay = onDay (day - 1) fishes
    zeros = fst . head . filter ((==0).snd) $ prevDay

    newFish (c, 7) = (c + zeros, 6)
    newFish (c, x) = (c, x - 1)
    restFishes = map newFish . filter ((/=0).snd) $ prevDay
    newFishes = restFishes ++ [(zeros, 8)]

findAnswer :: [Int] -> Int
findAnswer l = sum . map fst . onDay 256 . map (\x -> (occurrences x, x)) $ [0..8]
  where
    occurrences x = length.filter (==x) $ l

main :: IO ()
main = do
  line <- getLine
  let fishes = map read . splitOn "," $ line
  print . findAnswer $ fishes
