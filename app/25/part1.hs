import Data.Set as Set (Set, empty, union, insert, member, delete, map)
type State = (Set (Int, Int), Set (Int, Int))
type Bounds = (Int, Int)

locToEast :: Bounds -> (Int, Int) -> (Int, Int)
locToEast (mx, _) (x, y) = (if x+1 > mx then 0 else x+1, y)

locToSouth :: Bounds -> (Int, Int) -> (Int, Int)
locToSouth (_, my) (x, y) = (x, if y+1 > my then 0 else y+1)

blocked :: State -> (Int, Int) -> Bool
blocked (e, s) loc = loc `member` s || loc `member` e

moveEast :: Bounds -> State -> (Int, Int) -> (Int, Int)
moveEast bounds s loc = newEast
  where
    nLoc = locToEast bounds loc
    newEast = if blocked s nLoc then loc else nLoc

moveSouth :: Bounds -> State -> (Int, Int) -> (Int, Int)
moveSouth bounds s loc = newSouth
  where
    nLoc = locToSouth bounds loc
    newSouth = if blocked s nLoc then loc else nLoc

moveAllEast :: Bounds -> State -> State
moveAllEast bounds state@(e, s) = (ne, s)
  where
    ne = Set.map (moveEast bounds state) e

moveAllSouth :: Bounds -> State -> State
moveAllSouth bounds state@(e, s) = (e, ns)
  where
    ns = Set.map (moveSouth bounds state) s

step :: Bounds -> State -> State
step bounds state = movedSouth
  where
    movedEast = moveAllEast bounds state
    movedSouth = moveAllSouth bounds movedEast

steps :: Bounds -> State -> Int -> Int
steps bounds state c = if state == newState then c else steps bounds newState (c+1)
  where newState = step bounds state

findAnswer :: Bounds -> State -> Int
findAnswer bounds state = steps bounds state 1

addToState :: State -> (Int, Int) -> Char -> State
addToState (e, s) (x, y) '>' = (insert (x, y) e, s)
addToState (e, s) (x, y) 'v' = (e, insert (x, y) s)
addToState state _ _ = state

mergeState :: State -> State -> State
mergeState (e, s) (e', s') = (e `union` e', s `union` s')

main :: IO ()
main = do
  input <- getContents
  let ys = lines input
  let maxY = length ys - 1
  let maxX = length (head ys) - 1
  let xMapper y xs = foldl (\state (e, x) -> addToState state (x, y) e) (Set.empty, Set.empty) (zip xs [0..])
  let state = foldl (\state (yLine, y) -> mergeState state (xMapper y yLine)) (Set.empty, Set.empty) (zip ys [0..])
  print $ findAnswer (maxX, maxY) state
