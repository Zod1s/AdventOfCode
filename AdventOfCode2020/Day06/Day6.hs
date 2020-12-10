import System.IO
import Data.List.Split(splitOn)
import Data.List(nub, intersect)

main :: IO()
main = do
  input <- (filter (\x -> x /= "") . splitOn "\n\n") <$> readFile "input.txt"
  print $ part2 input

part1 :: [String] -> Int
part1 list = sum $ map (length . nub . filter (\x -> x /= '\n')) list

part2 :: [String] -> Int
part2 list = sum $ map (length . foldr1 intersect) answers
  where answers = map (filter (\x -> x /= "") . splitOn "\n") list
