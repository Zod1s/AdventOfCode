import System.IO
import Data.Char(digitToInt)

main :: IO()
main = do
    input <- lines <$> readFile "input.txt"
    print $ part1 input

part1 :: [String] -> Int
part1 = length . (filter cond1)

part2 :: [String] -> Int
part2 = length . (filter cond2)

cond1 :: String -> Bool
cond1 pass = len >= lower && len <= upper
    where (lo, up, c, rest) = format pass
          lower = convert lo
          upper = convert up
          len = length $ filter (\x -> x == c) rest

cond2 :: String -> Bool
cond2 pass = (rest !! (first - 1) == c) /= (rest !! (second - 1) == c)
    where (lo, up, c, rest) = format pass
          first = convert lo
          second = convert up

format :: String -> (String, String, Char, String)
format (m:'-':ma:' ':l:':':' ':rest) = ([m], [ma], l, rest)
format (m1:m2:'-':ma:' ':l:':':' ':rest) = ([m1,m2], [ma], l, rest)
format (m:'-':ma1:ma2:' ':l:':':' ':rest) = ([m], [ma1, ma2], l, rest)
format (m1:m2:'-':ma1:ma2:' ':l:':':' ':rest) = ([m1,m2], [ma1, ma2], l, rest)
format _ = undefined

convert :: String -> Int
convert [x] = digitToInt x
convert n@[x, y] = read n