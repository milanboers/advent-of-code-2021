import Data.List.Split (splitOn)
import Data.List (nub, intercalate)

type Pos = (Int, Int)
type Axis = String
type Fold = (Axis, Int)

foldDot :: Fold -> Pos -> Pos
foldDot ("x",n) dot@(x,y) = if (x-n) > 0 then (-x+2*n, y) else dot
foldDot ("y",n) dot@(x,y) = if (y-n) > 0 then (x, -y+2*n) else dot
foldDot _ _ = error "Wrong fold axis"

foldDots :: Fold -> [Pos] -> [Pos]
foldDots fold = nub . map (foldDot fold)

foldAll :: [Fold] -> [Pos] -> [Pos]
foldAll folds dots = foldl (flip foldDots) dots folds

dotsToOutput :: [Pos] -> String
dotsToOutput dots = intercalate "\n" lines
  where
    maxX = maximum $ map fst dots
    maxY = maximum $ map snd dots
    lines = [[if (x, y) `elem` dots then '%' else ' ' | x <- [0..maxX]] | y <- [0..maxY]]

findAnswer :: [Fold] -> [Pos] -> String
findAnswer folds dots = dotsToOutput $ foldAll folds dots

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let [dots', folds'] = splitOn [""] input
  let dots = map ((\[x, y] -> (read x, read y)) . splitOn ",") dots'
  let folds = map ((\[axis, n] -> (axis, read n)) . (splitOn "=" . drop 11)) folds'
  putStrLn $ findAnswer folds dots
