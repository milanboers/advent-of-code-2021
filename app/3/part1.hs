import Data.List (transpose)

mostFrequent :: [Bool] -> Bool
mostFrequent xs = trues > length xs - trues
  where
    trues = length . filter (==True) $ xs

toInt :: [Bool] -> Int
toInt xs = toInt' (reverse xs) 0
  where
    toInt' (x:xs) e = fromEnum x * 2^e + toInt' xs (e+1)
    toInt' [] _ = 0

findAnswer :: [[Bool]] -> Int
findAnswer xxs = gamma * epsilon
  where
    transposed = transpose xxs
    gammaBin = map mostFrequent transposed
    epsilonBin = map not gammaBin
    gamma = toInt gammaBin
    epsilon = toInt epsilonBin
  

main :: IO ()
main = do
  content <- getContents
  let input = (map . map) (=='1') . lines $ content
  print $ findAnswer input
