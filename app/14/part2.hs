import Data.Map as Map (fromList, Map, lookup, unionWith, empty, adjust, singleton)
import Data.List.Split (splitOn)
import Data.Function.Memoize (memoFix2)

type Rules = Map (Char, Char) Char

unionOccs :: Char -> Map Char Int -> Map Char Int -> Map Char Int
unionOccs x m1 m2 = adjust (subtract 1) x $ unionWith (+) m1 m2

atStep :: Rules -> Int -> (Char, Char) -> Map Char Int
atStep rules step p = memoFix2 atStep' step p
  where
    atStep' f 0 (c,d) = unionWith (+) (singleton c 1) (singleton d 1)
    atStep' f step (c,d) = case Map.lookup (c,d) rules of
      Just x -> unionOccs x (f (step-1) (c,x)) (f (step-1) (x,d))
      Nothing -> f 0 (c,d)

pairs :: String -> [(Char, Char)]
pairs (x:y:ys) = (x,y):pairs (y:ys)
pairs _ = []

findAnswer :: Rules -> String -> Int
findAnswer rules polymer = maximum finalOccs - minimum finalOccs
  where
    allIterated = atStep rules 40 <$> pairs polymer
    allMerged = foldl (unionWith (+)) Map.empty allIterated
    doubles = tail.init $ polymer
    finalOccs = foldl (flip (adjust (subtract 1))) allMerged doubles

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let (polymer:_:rules') = input
  let rules = map ((\[[a,b],[c]] -> ((a,b),c)) . splitOn " -> ") rules'
  print $ findAnswer (fromList rules) polymer
