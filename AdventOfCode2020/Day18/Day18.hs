import System.IO
import Control.Monad.ST
import Data.STRef

main :: IO()
main = do
    input <- (map (filter (\x -> x /= '\r' && x /= ' ')) . lines) <$> readFile "input.txt"
    print $ head input

-- reverse' :: String -> String
-- reverse' [] = []
-- reverse' [x] = if x == '(' then ")" else if x == ')' then "(" else [x]
-- reverse' (x:xs) = if x == '(' then (reverse' xs) ++ ")" else if x == ')' then (reverse' xs) ++ "(" else (reverse' xs) ++ [x]

part1 :: [String] -> Int
part1 str = undefined

eval :: String -> Int
eval str = 