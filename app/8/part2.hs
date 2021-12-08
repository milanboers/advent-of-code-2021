import Data.List.Split (splitOn)
import Data.List (intersect, sort, findIndex)
import Data.Maybe (fromJust)

type Segment = ([String], [String])

findAnswerForSegment :: Segment -> Int
findAnswerForSegment (patterns, output) = read . concatMap (show . digitForPattern) $ output
  where
    filterByIntersectionLength x l = filter ((==l).length.intersect (patternForDigit x))
    length5Patterns = filter ((==5).length) patterns
    length6Patterns = filter ((==6).length) patterns

    patternForDigit 0 = head . filterByIntersectionLength 5 4 $ length6Patterns
    patternForDigit 1 = head . filter ((==2).length) $ patterns
    patternForDigit 2 = head . filter (/= patternForDigit 3) . filterByIntersectionLength 6 4 $ length5Patterns
    patternForDigit 3 = head . filterByIntersectionLength 7 3 $ length5Patterns
    patternForDigit 4 = head . filter ((==4).length) $ patterns
    patternForDigit 5 = head . filter (/= patternForDigit 3) . filterByIntersectionLength 6 5 $ length5Patterns
    patternForDigit 6 = head . filterByIntersectionLength 1 1 $ length6Patterns
    patternForDigit 7 = head . filter ((==3).length) $ patterns
    patternForDigit 8 = head . filter ((==7).length) $ patterns
    patternForDigit 9 = head . filterByIntersectionLength 4 4 $ length6Patterns
    patternForDigit _ = error "Wrong digit"

    patternsForDigits = map patternForDigit [0..9]
    digitForPattern pattern = fromJust . findIndex ((==sort pattern).sort) $ patternsForDigits

findAnswer :: [Segment] -> Int
findAnswer = sum . map findAnswerForSegment

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let splitLines = map (splitOn "|") input
  let segments = map ((\[x, y] -> (x, y)) . map words) splitLines
  print $ findAnswer segments
