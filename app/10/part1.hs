
lineScore :: String -> Int
lineScore line = lineScore' (line ++ ".") "."
  where
    lineScore' (x:xs) (y:ys) | x == y = lineScore' xs ys
    lineScore' ('(':xs) l = lineScore' xs (')':l)
    lineScore' ('[':xs) l = lineScore' xs (']':l)
    lineScore' ('{':xs) l = lineScore' xs ('}':l)
    lineScore' ('<':xs) l = lineScore' xs ('>':l)
    lineScore' (')':xs) l = 3
    lineScore' (']':xs) l = 57
    lineScore' ('}':xs) l = 1197
    lineScore' ('>':xs) l = 25137
    lineScore' _ _ = 0

findAnswer :: [String] -> Int
findAnswer = sum . map lineScore

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  print $ findAnswer input
