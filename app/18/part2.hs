import Data.Char (digitToInt, isDigit)
import Debug.Trace (traceShow)

data Number = Pair Number Number | Num Int deriving (Eq)

instance Show Number where
  show (Num x) = show x
  show (Pair x y) = "[" ++ show x ++ "," ++ show y ++ "]"

breakInc :: (a -> Bool) -> [a] -> ([a], [a])
breakInc p l = case broken of
  (ls, r:rs) -> (ls ++ [r], rs)
  (ls, rs) -> (ls, rs)
  where
    broken = break p l

explode :: Number -> Number
explode num = case findExplodePoint 0 0 numString of
  Just i -> parseNumber $ explode' i numString
  Nothing -> num
  where
    numString = show num

    findExplodePoint _ _ [] = Nothing
    findExplodePoint 4 i ('[':xs) = Just i
    findExplodePoint depth i ('[':xs) = findExplodePoint (depth+1) (i+1) xs
    findExplodePoint depth i (']':xs) = findExplodePoint (depth-1) (i+1) xs
    findExplodePoint depth i (_:xs) = findExplodePoint depth (i+1) xs

    explode' explodePoint numString = exploded
      where
        (left, right') = splitAt explodePoint numString
        (pair, right) = breakInc (==']') right'
        (leftPre, leftPost') = break isDigit $ reverse left
        (rightPre, rightPost') = break isDigit right
  
        (Pair (Num pairLeft) (Num pairRight)) = parseNumber pair
        leftPart = case leftPost' of
          x:xs | isDigit x -> reverse leftPost ++ show (read (reverse leftNum) + pairLeft) ++ reverse leftPre
            where (leftNum, leftPost) = span isDigit leftPost'
          _ -> left
        rightPart = case rightPost' of
          x:xs | isDigit x -> rightPre ++ show (read rightNum + pairRight) ++ rightPost
            where (rightNum, rightPost) = span isDigit rightPost'
          _ -> right
        exploded = leftPart ++ "0" ++ rightPart

split :: Number -> Number
split = fst . split'
  where
    split' (Num x) | x > 9 = (Pair (Num d) (Num (d+m)), True)
      where (d, m) = x `divMod` 2
    split' (Pair left right) = if leftSplit then (Pair newLeft right, leftSplit) else (Pair left newRight, rightSplit)
      where
        (newLeft, leftSplit) = split' left
        (newRight, rightSplit) = split' right
    split' n = (n, False)

reduce :: Number -> Number
reduce n
  | reduced == n = reduced
  | otherwise = reduce reduced
  where
    exploded = explode n
    reduced = if exploded == n then split n else exploded

parseNumber :: String -> Number
parseNumber = fst . parseNumber'
  where
    parseNumber' :: String -> (Number, String)
    parseNumber' [] = error "Impossible"
    parseNumber' ('[':xs) = (Pair parsedFirst parsedSecond, rest)
      where
        (parsedFirst, rawSecond) = parseNumber' xs
        (parsedSecond, rest) = parseNumber' rawSecond
    parseNumber' (']':xs) = parseNumber' xs
    parseNumber' (',':xs) = parseNumber' xs
    parseNumber' xs = (Num (read numS), rest)
      where
        (numS, rest) = span isDigit xs

add :: Number -> Number -> Number
add num1 num2 = reduce $ Pair num1 num2

magnitude :: Number -> Int
magnitude (Num x) = x
magnitude (Pair left right) = 3 * magnitude left + 2 * magnitude right

findAnswer :: [Number] -> Int
findAnswer nums = maximum . map magnitude $ [add n1 n2 | n1 <- nums, n2 <- nums, n1 /= n2]

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let numbers = map parseNumber input
  print $ findAnswer numbers
