import Data.List.Split (splitWhen, splitOn)
import Data.List (find, nub, isPrefixOf, intersect)
import Data.Maybe (isJust)
import Data.Set as Set (fromList, size, intersection, Set, map, toList, union)

data Axis = X | Y | Z deriving (Show, Eq, Ord)
type Scanner = Set Measurement
type Measurement = (Int, Int, Int)
type PositionDelta = (Int, Int, Int)
type OrientationDelta = ((Axis, Bool), (Axis, Bool), (Axis, Bool))
type Delta = (PositionDelta, OrientationDelta)

mutWithPosDelta :: PositionDelta -> Measurement -> Measurement
mutWithPosDelta (dx,dy,dz) (mx,my,mz) = (mx+dx,my+dy,mz+dz)

eqWithPosDelta :: PositionDelta -> Measurement -> Measurement -> Bool
eqWithPosDelta d m1 m2 = m1 == mutWithPosDelta d m2

axisValue :: Axis -> Measurement -> Int
axisValue X (mx, _, _) = mx
axisValue Y (_, my, _) = my
axisValue Z (_, _, mz) = mz

mutWithOrientationDelta :: OrientationDelta -> Measurement -> Measurement
mutWithOrientationDelta (ox, oy, oz) m = (newAxisValue ox, newAxisValue oy, newAxisValue oz)
  where
    newAxisValue (axis, neg) = axisValue axis m * (if neg then -1 else 1)

eqWithOrientationDelta :: OrientationDelta -> Measurement -> Measurement -> Bool
eqWithOrientationDelta d m1 m2 = m1 == mutWithOrientationDelta d m2

mutWithDelta :: Delta -> Measurement -> Measurement
mutWithDelta (pd, od) m = mutWithPosDelta pd $ mutWithOrientationDelta od m

eqWithDelta :: Delta -> Measurement -> Measurement -> Bool
eqWithDelta d m1 m2 = m1 == mutWithDelta d m2

mutScannerWithDelta :: Delta -> Scanner -> Scanner
mutScannerWithDelta d = Set.map (mutWithDelta d)

getPosDelta :: Measurement -> Measurement -> PositionDelta
getPosDelta (m1x, m1y, m1z) (m2x, m2y, m2z) = (m1x-m2x,m1y-m2y,m1z-m2z)

orientationDeltas :: [OrientationDelta]
orientationDeltas = [
  ((X,True),(Z,False),(Y,False)),
  ((X,True),(Y,False),(Z,True)),
  ((X,True),(Z,True),(Y,True)),
  ((X,True),(Y,True),(Z,False)),
  ((X,False),(Z,False),(Y,True)),
  ((X,False),(Y,True),(Z,True)),
  ((X,False),(Z,True),(Y,False)),
  ((X,False),(Y,False),(Z,False)),
  ((Y,True),(Z,True),(X,False)),
  ((Y,True),(X,False),(Z,False)),
  ((Y,True),(X,True),(Z,True)),
  ((Y,True),(Z,False),(X,True)),
  ((Y,False),(Z,True),(X,True)),
  ((Y,False),(X,True),(Z,False)),
  ((Y,False),(Z,False),(X,False)),
  ((Y,False),(X,False),(Z,True)),
  ((Z,True),(Y,True),(X,True)),
  ((Z,True),(Y,False),(X,False)),
  ((Z,True),(X,False),(Y,True)),
  ((Z,True),(X,True),(Y,False)),
  ((Z,False),(Y,True),(X,False)),
  ((Z,False),(X,False),(Y,False)),
  ((Z,False),(X,True),(Y,True)),
  ((Z,False),(Y,False),(X,True))]

overlap :: Scanner -> Scanner -> Maybe Delta
overlap s1 s2 = find (\d -> size (s1 `intersection` mutScannerWithDelta d s2) >= 12) deltas
  where
    s1l = toList s1
    s2l = toList s2
    deltas = [(getPosDelta m1 (mutWithOrientationDelta od m2), od) | m1 <- s1l, m2 <- s2l, od <- orientationDeltas]

findDeltas :: [Scanner] -> [Delta]
findDeltas (x:xs) = findDeltas' xs x
  where
    findDeltas' :: [Scanner] -> Scanner -> [Delta]
    findDeltas' [] prevS = []
    findDeltas' xs prevS = delta : findDeltas' rest newS
      where
        (overlappingScanner, Just delta) = head $ filter (isJust.snd) $ Prelude.map (\s -> (s, overlap prevS s)) xs
        newS = prevS `union` mutScannerWithDelta delta overlappingScanner
        rest = filter (/=overlappingScanner) xs
findDeltas [] = error "Empty"

findAnswer :: [Scanner] -> Int
findAnswer scanners = maximum mhds
  where
    deltas = findDeltas scanners
    posDeltas = Prelude.map fst $ findDeltas scanners

    pairs = [(d1, d2) | d1 <- posDeltas, d2 <- posDeltas, d1 /= d2]
    mhds = Prelude.map (\((x1,y1,z1),(x2,y2,z2)) -> abs (x1-x2) + abs (y1-y2) + abs (z1-z2)) pairs

readMeasurement :: String -> (Int, Int, Int)
readMeasurement = (\[x,y,z] -> (x,y,z)) . Prelude.map read . splitOn ","

main :: IO ()
main = do
  contents <- getContents
  let input = filter (not.null) $ lines contents
  let scanners' = tail $ splitWhen ("---" `isPrefixOf`) input
  let scanners = Prelude.map (fromList . Prelude.map readMeasurement) scanners'
  print $ findAnswer scanners
