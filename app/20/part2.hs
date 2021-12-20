import Data.Vector (Vector, fromList, (!))
import Data.Matrix (Matrix, fromLists, safeGet, mapPos)
import Data.Maybe (fromMaybe)

type Algorithm = Vector Int
type Image = Matrix Int

toInt :: [Int] -> Int
toInt xs = toInt' (reverse xs) 0
  where
    toInt' (x:xs) e = x * 2^e + toInt' xs (e+1)
    toInt' [] _ = 0

steps :: Algorithm -> Image -> Int -> Image
steps algo image n = steps' image 0 n
  where
    steps' image _ 0 = image
    steps' image padding n = steps' newImage newPadding (n-1)
      where
        surroundings (r, c) = [fromMaybe padding $ safeGet nr nc image | nr <- [r-1..r+1], nc <- [c-1..c+1]]
        value (r,c) = algo ! toInt (surroundings (r,c))
        newImage = mapPos (\(r, c) v -> value (r,c)) image
        newPadding = algo ! toInt (replicate padding 9)

findAnswer :: Algorithm -> Image -> Int
findAnswer algo image = litPixels
  where
    imageAfterIters = steps algo image 50
    litPixels = sum imageAfterIters

rawToImage :: Int -> [[Int]] -> Image
rawToImage padding rawImage = image
  where
    lrPadding = replicate padding 0
    rawImage' = map (\line -> lrPadding ++ line ++ lrPadding) rawImage
    lineLength = length.head $ rawImage'
    tbPadding = replicate padding $ replicate lineLength 0
    rawImage'' = tbPadding ++ rawImage' ++ tbPadding
    image = fromLists rawImage''

main :: IO ()
main = do
  algoLine <- getLine
  _ <- getLine
  imageLines <- getContents
  let algo = fromList $ map (\c -> if c == '#' then 1 else 0) algoLine
  let rawImage = (map.map) (\c -> if c == '#' then 1 else 0) (lines imageLines)
  let image = rawToImage 50 rawImage
  print $ findAnswer algo image
