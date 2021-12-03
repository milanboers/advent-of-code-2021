import Data.List (transpose)

mostFrequent :: [Bool] -> Bool
mostFrequent xs = trues >= length xs - trues
  where 
    trues = length . filter (==True) $ xs

toInt :: [Bool] -> Int
toInt xs = toInt' (reverse xs) 0
  where
    toInt' (x:xs) e = fromEnum x * 2^e + toInt' xs (e+1)
    toInt' [] _ = 0

getValue :: ([Bool] -> Bool) -> [[Bool]] -> Int
getValue valueToKeep xxs = getValue' xxs 0
  where
    getValue' [x] _ = toInt x
    getValue' xxs pos = getValue' (filter f xxs) (pos + 1)
      where
        txxs = transpose xxs
        f xs = xs !! pos == valueToKeep (txxs !! pos)

findAnswer :: [[Bool]] -> Int
findAnswer xxs = oxygen * co2
  where
    oxygen = getValue mostFrequent xxs
    co2 = getValue (not.mostFrequent) xxs

main :: IO ()
main = do
  content <- getContents
  let input = (map . map) (=='1') . lines $ content
  print $ findAnswer input
