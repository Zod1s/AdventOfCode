import System.IO() 
import Data.Char(isDigit, digitToInt)
-- Use Map
data Instruction = Mask String
                 | Write Address Value
                 deriving (Show, Eq)

type Address = Int
type Value = String
type Memory = [(Address, Value)]

instance Read Instruction where
  readsPrec _ input =
    let instr = case input of
                  ('m':'a':'s':'k':' ':'=':' ':rest) -> Mask rest
                  ('m':'e':'m':rest) -> readWriteValue rest 
    in (\p -> [(instr, "")]) input

readWriteValue :: String -> Instruction
readWriteValue ('[':rest) = Write address value
  where address = read (takeWhile isDigit rest) :: Int
        (']':' ':'=':' ':r) = dropWhile isDigit rest
        value = decToBin (read r :: Integer)

decToBin, decToBin' :: Integer -> String
decToBin n = pad $ decToBin' n
  where pad str = (replicate (36 - l) '0') ++ str 
          where l = length str
decToBin' 0 = ""
decToBin' n = (decToBin' (div n 2)) ++ (show (rem n 2))

binToDec :: String -> Integer
binToDec b = foldl1 (\acc x -> x + 2 * acc) (map (toInteger . digitToInt) b)

mask :: String -> String -> String
mask m val = zipWith f m val
  where f '1' _ = '1'
        f '0' _ = '0'
        f 'X' x = x

part1 :: [Instruction] -> Integer
part1 instr = sum vals
  where mem = eval instr "" []
        vals = fmap ((\x -> binToDec x) . snd) mem

eval :: [Instruction] -> String -> Memory -> Memory
eval ((Mask m):instr) _ mem = eval instr m mem
eval ((Write address value):instr) curMask mem = case lookup address mem of
                                                   Just _ -> changeVal mem address newValue
                                                   Nothing -> eval instr curMask ((address, newValue) : mem)
  where newValue = mask curMask value

changeVal :: Memory -> Int -> String -> Memory
changeVal mem address value = fmap (\(x, y) -> if x == address then (address, value) else (x, y)) mem

main :: IO()
main = do
  input <- (map (\x -> read x :: Instruction) . lines) <$> readFile "input.txt"
  print $ part1 input
