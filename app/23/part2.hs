import Data.List (sort, find)
import Data.Maybe (maybeToList, fromJust)
import Data.Set as Set (Set, delete, size, filter, insert, fromList, toList, union)
import Data.MemoUgly (memo)
import Data.Function (fix)

data AmphipodType = A | B | C | D deriving (Show, Eq, Ord)
data AmphipodPos = Hallway Int | Room AmphipodType Int deriving (Show, Eq, Ord)
type Path = [AmphipodPos]
type Amphipod = (AmphipodType, AmphipodPos, Bool)
type State = Set Amphipod

roomSize :: Int
roomSize = 4

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

roomOccupants :: State -> AmphipodType -> Set Amphipod
roomOccupants s roomType = Set.filter (\(_, p, _) -> isInRoom p roomType) s
  where
    isInRoom (Room t _) t' = t == t'
    isInRoom _ _ = False

energy :: AmphipodType -> AmphipodPos -> AmphipodPos -> Int
energy t from to = energyFactor t * (abs (location from - location to) + roomCost from + roomCost to)
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
    destinations' = case size $ roomOccupants s t of
      ri | ri == roomSize -> []
      0 -> [Room t (roomSize-1)]
      ri -> [Room t (roomSize-1-ri)]
    destinations = Prelude.filter (not . pathBlocked s from) destinations'
moveTo s (t, from@(Room _ 0), False) = map (\to -> (energy t from to, (t,to,False))) destinations
  where
    destinations = Prelude.filter (not . pathBlocked s from) hallwayPoss
moveTo s (t, from@(Room fromRoomType roomPos), False) = map (\to -> (energy t from to, (t,to,False))) destinations
  where
    otherOccupants = roomOccupants s fromRoomType
    blockingOccupants = size $ Set.filter (\(_, Room _ ri, _) -> ri < roomPos) otherOccupants
    destinations = case blockingOccupants of
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

memoFix :: Ord a => ((a -> b) -> a -> b) -> a -> b
memoFix f = fix (memo . f)

minToEndState :: State -> Maybe Int
minToEndState = memoFix minToEndState'
  where
    minToEndState' f s
      | allInDestination = Just 0
      | otherwise = if (not.null) nextMoves then Just (minimum nextMoves) else Nothing
      where
        allInDestination = all isInDestination s
        nextStates = allNextStates s
        nextMoves = concatMap (\(nextState, energy) -> maybeToList $ (energy+) <$> f nextState) nextStates

findAnswer :: Int
findAnswer = fromJust $ minToEndState $ union startState addition
  where
    startState = fromList [(A, Room A 0, False), (B, Room A 3, False), (D, Room B 0, False), (C, Room B 3, False), (A, Room C 0, False), (D, Room C 3, False), (B, Room D 0, False), (C, Room D 3, False)]
    addition = fromList [(D, Room A 1, False), (D, Room A 2, False), (C, Room B 1, False), (B, Room B 2, False), (B, Room C 1, False), (A, Room C 2, False), (A, Room D 1, False), (C, Room D 2, False)]

main :: IO ()
main = do
  print findAnswer
