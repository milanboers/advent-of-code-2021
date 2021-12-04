
findAnswer :: [Int] -> Int
findAnswer (w:x:y:z:zs) = addition + findAnswer (x:y:z:zs)
  where
    addition = if x+y+z > w+x+y then 1 else 0
findAnswer _ = 0

main :: IO ()
main = do
  content <- getContents
  let input = map read . words $ content
  print $ findAnswer input
