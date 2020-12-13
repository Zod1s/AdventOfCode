import System.IO
import Data.List(sort, elemIndex)
import Data.List.Split(splitOn)
import Data.Maybe(fromJust)

main :: IO()
main = do
  input <- lines <$> readFile "input.txt"
  let timeId = read (input !! 0) :: Int
      -- noXbusIds = map (\x -> read x :: Int) $ filter (/="x") $ splitOn "," (input !! 1)
      busIds = zipWith (,) [0..] (map (\x -> if x /= "x" then read x :: Integer else 0) $ splitOn "," (input !! 1))
  print $ part2 (filter (\(_, x) -> x /= 0) busIds)

part1 :: Int -> [Int] -> Int
part1 time buss = delay * idBus
  where starts = map ((\x -> x - time) . head) [[x * i | i <- [1..], x * i >= time] | x <- buss]
        delay = minimum starts
        idIndex = fromJust $ elemIndex delay starts
        idBus = buss !! idIndex

part2 :: [(Integer, Integer)] -> Integer
part2 ((_, c):busIds) = cposs
  where poss = [[i - a | i <- [0, f..]] | (a, f) <- busIds]
        cposs = head [i | i <- [0, c..], and $ map (myIsElem i) poss]
        
-- cond :: [(Integer, Integer)] -> Integer -> Bool
-- cond list n = all id list'
--   where list' = [(rem (n + x) y) == 0 | (x, y) <- list]

myIsElem :: Integer -> [Integer] -> Bool
myIsElem i [] = False
myIsElem i (x:xs) = if x < i then myIsElem i xs else x == i
