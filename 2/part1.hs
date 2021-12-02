
findAnswer :: [(String, Int)] -> Int
findAnswer input = findAnswer' input (0, 0)
  where
    findAnswer' (("forward", x):xs) (pos, dep) = findAnswer' xs (pos + x, dep)
    findAnswer' (("down", x):xs) (pos, dep) = findAnswer' xs (pos, dep + x)
    findAnswer' (("up", x):xs) (pos, dep) = findAnswer' xs (pos, dep - x)
    findAnswer' _ (pos, dep) = pos * dep

main :: IO ()
main = do
  content <- getContents
  let input = map ((\[x, y] -> (x, read y)) . words) . lines $ content
  print $ findAnswer input
