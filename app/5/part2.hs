import Data.List.Split (splitOn)
import Data.List (sort, group)

type Point = (Int, Int)
type Line = (Point, Point)

fullLine :: Line -> [Point]
fullLine ((lx1, ly1), (lx2, ly2))
  | lx1 == lx2 = [(lx1, y) | y <- ys]
  | ly1 == ly2 = [(x, ly1) | x <- xs]
  | otherwise = zip xs ys
    where
      xs = [lx1..lx2] ++ reverse [lx2..lx1]
      ys = [ly1..ly2] ++ reverse [ly2..ly1]

findAnswer :: [Line] -> Int
findAnswer = length . filter ((>1).length) . group . sort . concatMap fullLine

main :: IO ()
main = do
  content <- getContents
  let input = lines content
  let readPoint p = (\[x, y] -> (x, y)) . map read . splitOn "," $ p :: Point
  let lines = (\[p1, p2] -> (p1, p2)) . map readPoint . splitOn "->" <$> input :: [Line]
  print $ findAnswer lines
