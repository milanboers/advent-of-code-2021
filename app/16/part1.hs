import Numeric (readHex)
import Text.Printf (printf)
import Data.Bifunctor (first)

type Version = Int
data Packet = Literal Version Int | Operator Version [Packet] deriving Show

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

readOperatorPacket :: Int -> [Bool] -> (Packet, [Bool])
readOperatorPacket _ [] = error "No packet"
readOperatorPacket version (lengthTypeId:xs) = (Operator version subpackets, rest)
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
  _ -> readOperatorPacket version rest
  where
    (rawHeader, rest) = splitAt 6 s
    (version, typeId) = readHeader rawHeader

hexToBits :: String -> [Bool]
hexToBits = concatMap hexCharToBits
  where
    hexCharToInt = fst . head . readHex . pure :: Char -> Int
    hexCharToBits = map (=='1') . printf "%04b" . hexCharToInt

deepVersionSum :: Packet -> Int
deepVersionSum (Literal v _) = v
deepVersionSum (Operator v ps) = v + sum (map deepVersionSum ps)

findAnswer :: String -> Int
findAnswer = deepVersionSum . fst . readPacket . hexToBits

main :: IO ()
main = do
  line <- getLine
  print $ findAnswer line