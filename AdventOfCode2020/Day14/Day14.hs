import System.IO() 
import Data.Char(isDigit, digitToInt)

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

mask1 :: String -> String -> String
mask1 m val = zipWith f m val
    where f '1' _ = '1'
          f '0' _ = '0'
          f 'X' x = x

part1 :: [Instruction] -> Integer
part1 instr = sum vals
    where mem = eval1 instr "" []
          vals = fmap ((\x -> binToDec x) . snd) mem

eval1 :: [Instruction] -> String -> Memory -> Memory
eval1 ((Mask m):instr) _ mem = eval1 instr m mem
eval1 ((Write address value):instr) curMask mem = 
    case lookup address mem of
        Just _ -> eval1 instr curMask newMem
        Nothing -> eval1 instr curMask ((address, newValue) : mem)
    where newValue = mask1 curMask value
          newMem = (changeVal mem address newValue)
eval1 _ _ mem = mem

changeVal :: Memory -> Int -> String -> Memory
changeVal mem address value = fmap (\(x, y) -> if x == address then (address, value) else (x, y)) mem

main :: IO()
main = do
    input <- (map (\x -> read x :: Instruction) . lines) <$> readFile "input.txt"
    print $ part1 input