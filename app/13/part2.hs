import Data.List.Split (splitOn)
import Data.List (nub, intercalate)
import Data.Set as Set (Set, fromList, map, member)

type Pos = (Int, Int)
type Axis = String
type Fold = (Axis, Int)

foldDot :: Fold -> Pos -> Pos
foldDot ("x",n) dot@(x,y) = if (x-n) > 0 then (-x+2*n, y) else dot
foldDot ("y",n) dot@(x,y) = if (y-n) > 0 then (x, -y+2*n) else dot
foldDot _ _ = error "Wrong fold axis"

foldDots :: Fold -> Set Pos -> Set Pos
foldDots fold = Set.map (foldDot fold)

foldAll :: [Fold] -> Set Pos -> Set Pos
foldAll folds dots = foldl (flip foldDots) dots folds

dotsToOutput :: Set Pos -> String
dotsToOutput dots = intercalate "\n" lines
  where
    maxX = maximum $ Set.map fst dots
    maxY = maximum $ Set.map snd dots
    lines = [[if (x, y) `member` dots then '%' else ' ' | x <- [0..maxX]] | y <- [0..maxY]]

findAnswer :: [Fold] -> Set Pos -> String
findAnswer folds dots = dotsToOutput $ foldAll folds dots

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let [dots', folds'] = splitOn [""] input
  let dots = Prelude.map ((\[x, y] -> (read x, read y)) . splitOn ",") dots'
  let folds = Prelude.map ((\[axis, n] -> (axis, read n)) . (splitOn "=" . drop 11)) folds'
  putStrLn $ findAnswer folds (fromList dots)
