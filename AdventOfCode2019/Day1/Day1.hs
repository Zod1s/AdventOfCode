import System.IO

main :: IO()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ part2 (lines contents)

part1 :: [String] -> Int
part1 txt = sum $ map (\x -> (div (read x) 3) - 2) txt

part2 :: [String] -> Int
part2 txt = sum $ map f masses
  where masses = map (\x -> (div (read x) 3) - 2) txt
        f mass = if fuel <= 0
                 then mass
                 else mass + f fuel
          where fuel = (div mass 3) - 2
