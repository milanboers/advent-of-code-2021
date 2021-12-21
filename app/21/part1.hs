import Data.List.Split (splitOn)

type Die = (Int, Int)
type Player = (Int, Int)

roll :: Die -> Die
roll (counter, prevPos) = (counter + 1, newVal)
  where
    newVal = (prevPos `mod` 100) + 1

move :: Player -> Int -> Player
move (pos, score) x = (newPos, newScore)
  where
    newPos = ((pos-1+x) `mod` 10) + 1
    newScore = score + newPos

rollAndMove :: Die -> Player -> (Die, Player)
rollAndMove die p = (last rolls, move p rollVals)
  where
    rolls = take 4 $ iterate roll die
    rollVals = sum . map snd $ init rolls

gameOutcome :: Die -> Player -> Player -> (Die, Player, Player)
gameOutcome die p1 p2 = case (newP1, newP2) of
  _ | snd newP1 >= 1000 -> (d1, newP1, p2)
  _ | snd newP2 >= 1000 -> (d2, newP2, newP1)
  _ -> gameOutcome d2 newP1 newP2
  where
    (d1, newP1) = rollAndMove die p1
    (d2, newP2) = rollAndMove d1 p2

findAnswer :: Int -> Int -> Int
findAnswer p1pos p2pos = fst die * snd loser
  where
    dieStart = (0, 1)
    p1 = (p1pos, 0)
    p2 = (p2pos, 0)
    (die, winner, loser) = gameOutcome dieStart p1 p2

main :: IO ()
main = do
  p1l <- getLine
  p2l <- getLine
  let [_, p1s] = splitOn ":" p1l
  let [_, p2s] = splitOn ":" p2l
  let p1 = read p1s
  let p2 = read p2s
  print $ findAnswer p1 p2
