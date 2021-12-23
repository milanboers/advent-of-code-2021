import Data.List (sort)
import Data.Maybe (maybeToList, fromJust)
import Data.Set as Set (Set, delete, size, filter, insert, fromList, toList)
import Data.MemoTrie (memoFix)

data AmphipodType = A | B | C | D deriving (Show, Eq, Ord, Enum)
data AmphipodPos = Hallway Int | Room AmphipodType Int deriving (Show, Eq, Ord)
type Path = [AmphipodPos]
type Amphipod = (AmphipodType, AmphipodPos, Bool)
type State = Set Amphipod

location :: AmphipodPos -> Int
location (Hallway i) = i
location (Room A _) = 2
location (Room B _) = 4
location (Room C _) = 6
location (Room D _) = 8

hallwayPoss :: [AmphipodPos]
hallwayPoss = map Hallway [0,1,3,5,7,9,10]

pathBlocked :: State -> AmphipodPos -> AmphipodPos -> Bool
pathBlocked s from to = any (\(_, loc, _) -> blocksPath loc fromLoc toLoc) s
  where
    fromLoc = location from
    toLoc = location to
    onPath i fi ti
      | fi < ti = i > fi && i <= ti
      | fi > ti = i < fi && i >= ti
      | otherwise = error "Impossible"
    blocksPath (Hallway a) fromLoc toLoc = onPath a fromLoc toLoc
    blocksPath _ _ _ = False

roomOccupants :: State -> AmphipodType -> Int
roomOccupants s roomType = size $ Set.filter (\(_, p, _) -> isInRoom p roomType) s
  where
    isInRoom (Room t _) t' = t == t'
    isInRoom _ _ = False

energy :: AmphipodType -> AmphipodPos -> AmphipodPos -> Int
energy t from to = energyFactor t * (abs (location from - location to) + roomCost to + roomCost from)
  where
    roomCost (Room _ i) = i+1
    roomCost _ = 0
    energyFactor A = 1
    energyFactor B = 10
    energyFactor C = 100
    energyFactor D = 1000

moveTo :: State -> Amphipod -> [(Int, Amphipod)]
moveTo s (t, from@(Hallway i), False) = map (\to -> (energy t from to, (t,to,True))) destinations
  where
    destinations' = case roomOccupants s t of
      0 -> [Room t 1]
      1 -> [Room t 0]
      _ -> []
    destinations = Prelude.filter (not . pathBlocked s from) destinations'
moveTo s (t, from@(Room _ 0), False) = map (\to -> (energy t from to, (t,to,False))) destinations
  where
    destinations = Prelude.filter (not . pathBlocked s from) hallwayPoss
moveTo s (t, from@(Room fromRoomType 1), False) = map (\to -> (energy t from to, (t,to,False))) destinations
  where
    destinations = case roomOccupants s fromRoomType of
      0 -> Prelude.filter (not . pathBlocked s from) hallwayPoss
      _ -> []
moveTo _ _ = []

allNextStates :: State -> [(State, Int)]
allNextStates s = concatMap nextStates s
  where
    nextStates (_, _, True) = []
    nextStates a = map (\(energy, nextA) -> (insert nextA sWithoutA, energy)) (moveTo sWithoutA a)
      where
        sWithoutA = delete a s

isInDestination :: Amphipod -> Bool
isInDestination (t, Room t' _, _) = t == t'
isInDestination _ = False

minToEndState :: State -> Maybe Int
minToEndState s = memoFix minToEndState' (toMemo s)
  where
    typeToMemo :: AmphipodType -> Int
    typeToMemo t = fromEnum t
    typeFromMemo :: Int -> AmphipodType
    typeFromMemo t = toEnum t
    posToMemo :: AmphipodPos -> (Int, Int)
    posToMemo (Hallway i) = (-1, i)
    posToMemo (Room a b) = (typeToMemo a, b)
    posFromMemo :: (Int, Int) -> AmphipodPos
    posFromMemo (-1, i) = Hallway i
    posFromMemo (a, b) = Room (typeFromMemo a) b
    podToMemo :: Amphipod -> (Int, (Int, Int), Bool)
    podToMemo (t, p, b) = (typeToMemo t, posToMemo p, b)
    podFromMemo :: (Int, (Int, Int), Bool) -> Amphipod
    podFromMemo (t, p, b) = (typeFromMemo t, posFromMemo p, b)
    toMemo :: State -> [(Int, (Int, Int), Bool)]
    toMemo s = sort $ map podToMemo (toList s)
    fromMemo :: [(Int, (Int, Int), Bool)] -> State
    fromMemo s = fromList $ map podFromMemo s

    minToEndState' f memoS
      | allInDestination = Just 0
      | otherwise = if (not.null) nextMoves then Just (minimum nextMoves) else Nothing
      where
        s = fromMemo memoS

        allInDestination = all isInDestination s
        nextStates = allNextStates s
        nextMoves = concatMap (\(nextState, energy) -> maybeToList $ (energy+) <$> f (toMemo nextState)) nextStates

findAnswer :: Int
findAnswer = fromJust $ minToEndState startState
  where
    --startState = fromList [(B, Room A 0, False), (A, Room A 1, False), (C, Room B 0, False), (D, Room B 1, False), (B, Room C 0, False), (C, Room C 1, False), (D, Room D 0, False), (A, Room D 1, False)]
    startState = fromList [(A, Room A 0, False), (B, Room A 1, False), (D, Room B 0, False), (C, Room B 1, False), (A, Room C 0, False), (D, Room C 1, False), (B, Room D 0, False), (C, Room D 1, False)]

main :: IO ()
main = do
  print findAnswer
