import System.IO

data Token = ADD Token Token
           | MUL Token Token
           | OPR Token
           | CPR
           | INT Int

main :: IO()
main = do
    input <- (map (filter (\x -> x /= '\r' && x /= ' ')) . lines) <$> readFile "input.txt"
    print input

part1 :: [String] -> [Int]
part1 ls = undefined

eval :: String -> Int
eval str = undefined

parse :: String -> Token
parse xs = undefined
-- foldr con acc lista di int fatta a stack