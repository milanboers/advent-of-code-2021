import Data.List (isPrefixOf, delete)
import Data.List.Split (splitOn)

data Axis = X | Y | Z deriving (Show, Eq)
type Cuboid = ((Int, Int), (Int, Int), (Int, Int))

saneCuboid :: Cuboid -> Bool
saneCuboid ((xb, xe), (yb, ye), (zb, ze)) = xb <= xe && yb <= ye && zb <= ze

filterSane :: [Cuboid] -> [Cuboid]
filterSane = filter saneCuboid

splitCuboidAxis :: (Axis, Int) -> Cuboid -> [Cuboid]
splitCuboidAxis (X, split) ((xb, xe), y, z) = filterSane [((xb, min split xe), y, z), ((max split xb, xe), y, z)]
splitCuboidAxis (Y, split) (x, (yb, ye), z) = filterSane [(x, (yb, min split ye), z), (x, (max split yb, ye), z)]
splitCuboidAxis (Z, split) (x, y, (zb, ze)) = filterSane [(x, y, (zb, min split ze)), (x, y, (max split zb, ze))]

intersection :: Cuboid -> Cuboid -> Maybe Cuboid
intersection ((xb, xe), (yb, ye), (zb, ze)) ((xb', xe'), (yb', ye'), (zb', ze'))
  | saneCuboid intersectionCuboid = Just intersectionCuboid
  | otherwise = Nothing
  where
    intersectionCuboid = ((max xb xb', min xe xe'), (max yb yb', min ye ye'), (max zb zb', min ze ze'))

splitCuboid :: Cuboid -> Cuboid -> [Cuboid]
splitCuboid c co@((coxb, coxe), (coyb, coye), (cozb, coze)) = 
  foldl (\cboids splitPoint -> concatMap (splitCuboidAxis splitPoint) cboids) [c] splitPoints
  where
    splitPoints = [(X, coxb), (X, coxe), (Y, coyb), (Y, coye), (Z, cozb), (Z, coze)]

turnOff :: Cuboid -> Cuboid -> [Cuboid]
turnOff c co = case intersection c co of
  Nothing -> [c]
  Just i -> i `delete` splitCuboid c co

takeAction :: (Cuboid, Bool) -> [Cuboid] -> [Cuboid]
takeAction (cboid, True) cboids = cboid : concatMap (`turnOff` cboid) cboids
takeAction (cboid, False) cboids = concatMap (`turnOff` cboid) cboids

cuboidSize :: Cuboid -> Int
cuboidSize ((xb, xe), (yb, ye), (zb, ze)) = (xe-xb) * (ye-yb) * (ze-zb)

findAnswer :: [(Cuboid, Bool)] -> Int
findAnswer actions = sum $ map cuboidSize afterActions
  where
    inRange (b,e) = b >= -50 && e <= 51
    filteredActions = filter (\((x,y,z),_) -> inRange x && inRange y && inRange z) actions
    afterActions = foldl (flip takeAction) [] filteredActions

parseCuboid :: String -> (Cuboid, Bool)
parseCuboid xs = (((xb, xe), (yb, ye), (zb, ze)), on)
  where
    on = "on" `isPrefixOf` xs
    [xR, yR, zR] = splitOn "," xs
    readRange = (\[s1, s2] -> (read s1, read s2 + 1)) . splitOn ".." . last . splitOn "="
    (xb, xe) = readRange xR
    (yb, ye) = readRange yR
    (zb, ze) = readRange zR

main :: IO ()
main = do
  input <- getContents
  let cuboidLines = lines input
  let cuboids = map parseCuboid cuboidLines
  print $ findAnswer cuboids
