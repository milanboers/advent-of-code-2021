{-# LANGUAGE TupleSections #-}
import Data.List.Split (splitOn)
import Data.List (transpose, partition)

type BingoCard = [[Int]]
type MarkedBingoCard = [[(Int, Bool)]]

isWinningCard :: MarkedBingoCard -> Bool
isWinningCard card = any (all snd) card || any (all snd) (transpose card)

cardScore :: MarkedBingoCard -> Int
cardScore = sum . map fst . filter (not.snd) . concat

markCard :: Int -> MarkedBingoCard -> MarkedBingoCard
markCard draw = map . map $ \(x, v) -> (x, v || draw == x)

markCards :: Int -> [MarkedBingoCard] -> [MarkedBingoCard]
markCards = map . markCard

findAnswer :: [BingoCard] -> [Int] -> Int
findAnswer cards = findAnswer' startingCards 0
  where
    startingCards = (map.map.map) (, False) cards
    findAnswer' markedCards lastWinnerScore (x:xs) = case winners of
      (winner:_) -> findAnswer' nonWinners (cardScore winner*x) xs
      [] -> findAnswer' nonWinners lastWinnerScore xs
      where
        newMarkedCards = markCards x markedCards
        (winners, nonWinners) = partition isWinningCard newMarkedCards
    findAnswer' _ lastWinnerScore [] = lastWinnerScore

main :: IO ()
main = do
  content <- getContents
  let input = lines content
  let (drawLines:cardsLines) = input
  let draws = map read . splitOn "," $ drawLines
  let toCard = map (map read . words)
  let cards = map toCard . filter (not.null) . splitOn [""] $ cardsLines
  print . findAnswer cards $ draws
