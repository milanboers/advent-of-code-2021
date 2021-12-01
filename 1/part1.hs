
findAnswer :: [Int] -> Int
findAnswer (x:y:ys) = addition + findAnswer (y:ys)
  where
    addition = if y > x then 1 else 0
findAnswer _ = 0

main :: IO ()
main = do
  content <- getContents
  let input = map read . words $ content
  print $ findAnswer input
