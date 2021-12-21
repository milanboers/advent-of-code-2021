import Data.List.Split (splitOn)
import Data.List (partition)
import Data.Function.Memoize (memoize, memoFix2)

type Die = Int
type Player = (Int, Int)

rolls :: [Int]
rolls = [sum [r1,r2,r3] | r1 <- [1..3], r2 <- [1..3], r3 <- [1..3]]

move :: Player -> Int -> Player
move (pos, score) x = (newPos, newScore)
  where
    newPos = ((pos-1+x) `mod` 10) + 1
    newScore = score + newPos

rollAndMove :: Player -> [Player]
rollAndMove = memoize rollAndMove'
  where
    rollAndMove' p =  map (move p) rolls

gameStateToWins :: (Player, Player) -> Bool -> (Int, Int, [(Player, Player)])
gameStateToWins = memoFix2 gameStateToWins'
  where
    mergeGames (p1w, p2w, gs) (p1w', p2w', gs') = (p1w+p1w', p2w+p2w', gs++gs')
    gameStateToWins' f (p1, p2) True = nextStates
      where
        p1rolls = rollAndMove p1
        (p1wins, rest) = partition ((>=21).snd) p1rolls
        nextGames = [f (newP1, p2) False | newP1 <- rest]
        nextStates = foldl mergeGames (length p1wins,0,[]) nextGames
    gameStateToWins' f (p1, p2) False = nextStates
      where
        p2rolls = rollAndMove p2
        (p2wins, rest) = partition ((>=21).snd) p2rolls
        nextGames = [f (p1, newP2) True | newP2 <- rest]
        nextStates = foldl mergeGames (0,length p2wins,[]) nextGames

findAnswer :: Int -> Int -> Int
findAnswer p1pos p2pos = p1wins
  where
    (p1wins, p2wins, rest) = gameStateToWins ((p1pos, 0), (p2pos, 0)) True

main :: IO ()
main = do
  p1l <- getLine
  p2l <- getLine
  let [_, p1s] = splitOn ":" p1l
  let [_, p2s] = splitOn ":" p2l
  let p1 = read p1s
  let p2 = read p2s
  print $ findAnswer p1 p2
