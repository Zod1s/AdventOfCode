import System.IO

main :: IO()
main = do
  input <- lines <$> readFile "input.txt"
  print input

part1 :: [String] -> Int
part1 = sum . (map eval)

eval :: String -> Int
eval expr = undefined
