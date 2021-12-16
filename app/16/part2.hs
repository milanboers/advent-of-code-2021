import Numeric (readHex)
import Text.Printf (printf)
import Data.Bifunctor (first)

type Version = Int
type TypeId = Int
data Packet = Literal Version Int | Operator Version TypeId [Packet] deriving Show

toInt :: [Bool] -> Int
toInt xs = toInt' (reverse xs) 0
  where
    toInt' (x:xs) e = fromEnum x * 2^e + toInt' xs (e+1)
    toInt' [] _ = 0

readSubpackets :: (([Packet], [Bool]) -> Bool) -> ([Packet], [Bool]) -> ([Packet], [Bool])
readSubpackets stopCondition input@(formerPackets, rest)
  | stopCondition input = input
  | otherwise = readSubpackets stopCondition (formerPackets ++ [newPacket], newRest)
  where
    (newPacket, newRest) = readPacket rest

readSubpacketsType0 :: [Bool] -> ([Packet], [Bool])
readSubpacketsType0 s = (subpackets, rest)
  where
    (rawSubLength, rest') = splitAt 15 s
    subLength = toInt rawSubLength
    (rawSubpackets, rest) = splitAt subLength rest'

    (subpackets, _) = readSubpackets (null.snd) ([], rawSubpackets)

readSubpacketsType1 :: [Bool] -> ([Packet], [Bool])
readSubpacketsType1 s = (subpackets, rest)
  where
    (rawNSubs, rest') = splitAt 11 s
    nSubs = toInt rawNSubs

    (subpackets, rest) = readSubpackets ((==nSubs).length.fst) ([], rest')

readLiteral :: [Bool] -> (Int, [Bool])
readLiteral = first toInt . literalToBits
  where
    literalToBits :: [Bool] -> ([Bool], [Bool])
    literalToBits [] = ([], [])
    literalToBits (True:xs) = (v ++ parseResult, rest)
      where
        (v, ys) = splitAt 4 xs
        (parseResult, rest) = literalToBits ys
    literalToBits (False:xs) = splitAt 4 xs

readLiteralPacket :: Int -> [Bool] -> (Packet, [Bool])
readLiteralPacket version s = (Literal version literal, rest)
  where
    (literal, rest) = readLiteral s

readOperatorPacket :: Int -> Int -> [Bool] -> (Packet, [Bool])
readOperatorPacket _ _ [] = error "No packet"
readOperatorPacket version typeId (lengthTypeId:xs) = (Operator version typeId subpackets, rest)
  where
    subPacketReader = if lengthTypeId then readSubpacketsType1 else readSubpacketsType0
    (subpackets, rest) = subPacketReader xs

readHeader :: [Bool] -> (Int, Int)
readHeader s = (toInt version, toInt typeId)
  where
    (version, typeId) = splitAt 3 s

readPacket :: [Bool] -> (Packet, [Bool])
readPacket s = case typeId of
  4 -> readLiteralPacket version rest
  typeId -> readOperatorPacket version typeId rest
  where
    (rawHeader, rest) = splitAt 6 s
    (version, typeId) = readHeader rawHeader

hexToBits :: String -> [Bool]
hexToBits = concatMap hexCharToBits
  where
    hexCharToInt = fst . head . readHex . pure :: Char -> Int
    hexCharToBits = map (=='1') . printf "%04b" . hexCharToInt

packetValue :: Packet -> Int
packetValue (Literal _ v) = v
packetValue (Operator _ 0 ps) = sum $ map packetValue ps
packetValue (Operator _ 1 ps) = product $ map packetValue ps
packetValue (Operator _ 2 ps) = minimum $ map packetValue ps
packetValue (Operator _ 3 ps) = maximum $ map packetValue ps
packetValue (Operator _ 5 [p1, p2]) = fromEnum $ packetValue p1 > packetValue p2
packetValue (Operator _ 6 [p1, p2]) = fromEnum $ packetValue p1 < packetValue p2
packetValue (Operator _ 7 [p1, p2]) = fromEnum $ packetValue p1 == packetValue p2
packetValue _ = error "Wrong packet"

findAnswer :: String -> Int
findAnswer = packetValue . fst . readPacket . hexToBits

main :: IO ()
main = do
  line <- getLine
  print $ findAnswer line