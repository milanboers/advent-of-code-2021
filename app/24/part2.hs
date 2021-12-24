import Data.Map as Map (Map, fromList, insert, lookup)
import Data.Maybe (fromJust, mapMaybe, listToMaybe)
import Text.Read (readMaybe)

type State = Map Char Int
type Input = [Int]
data Term = Var Char | Lit Int deriving (Show, Eq, Ord)
data Instruction = Input Char | Add Char Term | Mul Char Term | Div Char Term | Mod Char Term | Eql Char Term deriving (Show, Eq, Ord)

getVar :: State -> Char -> Int
getVar vars x = fromJust $ Map.lookup x vars

termValue :: State -> Term -> Int
termValue _ (Lit x) = x
termValue vars (Var x) = getVar vars x

doIntOper :: State -> (Int -> Int -> Int) -> Char -> Term -> State
doIntOper vars oper a b = insert a newAVal vars
  where
    aVal = getVar vars a
    bVal = termValue vars b
    newAVal = oper aVal bVal

possibleInputs :: Int -> Int -> [Int]
possibleInputs ic z
  | ic == 5 = withinRange $ z `mod` 26 - 1
  | ic == 7 = withinRange $ z `mod` 26 - 8
  | ic == 8 = withinRange $ z `mod` 26 - 7
  | ic == 9 = withinRange $ z `mod` 26 - 8
  | ic == 11 = withinRange $ z `mod` 26 - 2
  | ic == 12 = withinRange $ z `mod` 26 - 2
  | ic == 13 = withinRange $ z `mod` 26 - 13
  | otherwise = range
  where
    range = [1..9]
    withinRange n = [n | n < 10 && n > 0]

exec :: State -> Input -> [Instruction] -> [Input]
exec vars input [] = [input | getVar vars 'z' == 0]
exec vars input (Input a:is) = branches
  where
    ic = length input
    z = getVar vars 'z'
    branches = concatMap (\newInput -> exec (insert a newInput vars) (input ++ [newInput]) is) (possibleInputs ic z)
exec vars input (Add a b:is) = exec (doIntOper vars (+) a b) input is
exec vars input (Mul a b:is) = exec (doIntOper vars (*) a b) input is
exec vars input (Div a b:is) = exec (doIntOper vars div a b) input is
exec vars input (Mod a b:is) = exec (doIntOper vars mod a b) input is
exec vars input (Eql a b:is) = exec (doIntOper vars eql a b) input is
  where
    eql a b = fromEnum $ a == b

validInputs :: [Instruction] -> [Input]
validInputs = exec initialVars []
  where
    initialVars = fromList [('w',0),('x',0),('y',0),('z',0)]

findAnswer :: [Instruction] -> Int
findAnswer = read . concatMap show . head . validInputs

parseTerm :: String -> Term
parseTerm s = case readMaybe s of
  Just i -> Lit i
  Nothing -> Var (head s)

parseLine :: String -> Instruction
parseLine ['i','n','p',' ',a] = Input a
parseLine ('a':'d':'d':' ':a:' ':b) = Add a (parseTerm b)
parseLine ('m':'u':'l':' ':a:' ':b) = Mul a (parseTerm b)
parseLine ('d':'i':'v':' ':a:' ':b) = Div a (parseTerm b)
parseLine ('m':'o':'d':' ':a:' ':b) = Mod a (parseTerm b)
parseLine ('e':'q':'l':' ':a:' ':b) = Eql a (parseTerm b)
parseLine i = error $ "Unknown instruction " ++ i

main :: IO ()
main = do
  input <- getContents
  let instructions = map parseLine $ lines input
  print $ findAnswer instructions
