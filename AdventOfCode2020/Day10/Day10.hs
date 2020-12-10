import System.IO
import Data.List(sort)

main :: IO()
main = do
  input <- (map (\x -> (read x :: Int)) . lines) <$> readFile "input.txt"
  print $ part1 input

part1 :: [Int] -> Int
part1 list = (l1 + 1) * l3
  where m = maximum list
        list' = (sort list) ++ [m+3]
        toCount = zipWith (-) (tail list') list'
        l1 = length $ filter (==1) toCount
        l3 = length $ filter (==3) toCount
