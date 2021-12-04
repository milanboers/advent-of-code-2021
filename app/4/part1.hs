{-# LANGUAGE TupleSections #-}
import Data.List.Split (splitOn)
import Data.List (find, transpose)

type BingoCard = [[Int]]
type MarkedBingoCard = [[(Int, Bool)]]

isWinningCard :: MarkedBingoCard -> Bool
isWinningCard = any (all snd)

cardScore :: MarkedBingoCard -> Int
cardScore = sum . map fst . filter (not.snd) . concat

markCard :: Int -> MarkedBingoCard -> MarkedBingoCard
markCard draw = map . map $ \(x, v) -> (x, v || draw == x)

markCards :: Int -> [MarkedBingoCard] -> [MarkedBingoCard]
markCards = map . markCard

findAnswer :: [BingoCard] -> [Int] -> Int
findAnswer cards = findAnswer' startingCards
  where
    startingCards = (map.map.map) (, False) cards
    findAnswer' markedCards (x:xs) = case find isWinningCard newMarkedCards of
      Just winner -> cardScore winner * x
      Nothing -> findAnswer' newMarkedCards xs
      where
        newMarkedCards = markCards x markedCards
    findAnswer' _ [] = error "Seen all cards, no winner"

main :: IO ()
main = do
  content <- getContents
  let input = lines content
  let (drawLines:cardsLines) = input
  let draws = map read . splitOn "," $ drawLines
  let toCard = map (map read . words)
  let normalCards = map toCard . splitOn [""] $ cardsLines
  let transposedCards = map transpose normalCards
  let cards = normalCards ++ transposedCards
  print $ findAnswer cards draws
