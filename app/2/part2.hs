
findAnswer :: [(String, Int)] -> Int
findAnswer input = findAnswer' input 0 (0, 0)
  where
    findAnswer' (("forward", x):xs) aim (pos, dep) = findAnswer' xs aim (pos + x, dep + aim * x)
    findAnswer' (("down", x):xs) aim loc = findAnswer' xs (aim + x) loc
    findAnswer' (("up", x):xs) aim loc = findAnswer' xs (aim - x) loc
    findAnswer' _ _ (pos, dep) = pos * dep

main :: IO ()
main = do
  content <- getContents
  let input = map ((\[x, y] -> (x, read y)) . words) . lines $ content
  print $ findAnswer input
