import Data.List.Split (splitOn)

type Segment = ([String], [String])

findAnswerForSegment :: Segment -> Int
findAnswerForSegment = length . filter ((`elem` [2, 3, 4, 7]).length) . snd

findAnswer :: [Segment] -> Int
findAnswer = sum . map findAnswerForSegment

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let splitLines = map (splitOn "|") input
  let segments = map ((\[x, y] -> (x, y)) . map words) splitLines
  print $ findAnswer segments
