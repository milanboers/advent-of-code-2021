import Data.List (sort)

charScore :: Char -> Int
charScore ')' = 1
charScore ']' = 2
charScore '}' = 3
charScore '>' = 4
charScore _ = error "Illegal char."

completionScore :: String -> Int
completionScore = foldl (\acc c -> acc*5 + charScore c) 0 . init

lineScore :: String -> Int
lineScore line = lineScore' (line ++ ".") "."
  where
    lineScore' "." xs = completionScore xs
    lineScore' (x:xs) (y:ys) | x == y = lineScore' xs ys
    lineScore' ('(':xs) l = lineScore' xs (')':l)
    lineScore' ('[':xs) l = lineScore' xs (']':l)
    lineScore' ('{':xs) l = lineScore' xs ('}':l)
    lineScore' ('<':xs) l = lineScore' xs ('>':l)
    lineScore' _ _ = 0

median :: [Int] -> Int
median l = sort l !! (length l `div` 2)

findAnswer :: [String] -> Int
findAnswer = median . filter (/=0) . map lineScore

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  print $ findAnswer input
