import System.IO
import Data.List(sort, elemIndex)
import Data.List.Split(splitOn)

main :: IO()
main = do
    input <- lines <$> readFile "input.txt"
    let timeId = read (input !! 0) :: Int
    print $ part1 timeId input

part1 :: Int -> [String] -> Int
part1 time list = part1' time (map (\x -> read x :: Int) $ filter (/="x") $ splitOn "," (list !! 1))

part1' :: Int -> [Int] -> Int
part1' time buss = delay * idBus
    where starts = map ((\x -> x - time) . head) [[x * i | i <- [1..], x * i >= time] | x <- buss]
          delay = minimum starts
          (Just idIndex) = elemIndex delay starts
          idBus = buss !! idIndex

part2 :: [String] -> Integer
part2 list = part2' $ filter (\(_, x) -> x /= 0) 
                             (zipWith (,) [0..]
                                      (map (\x -> if x /= "x" then read x :: Integer else 0)
                                      (splitOn "," (list !! 1))))

part2' :: [(Integer, Integer)] -> Integer
part2' ((_, c):busIds) = cposs
    where poss = [[i - a | i <- [0, f..]] | (a, f) <- busIds]
          cposs = head [i | i <- [0, c..], and $ map (myIsElem i) poss]

myIsElem :: Integer -> [Integer] -> Bool
myIsElem i [] = False
myIsElem i (x:xs) = if x < i then myIsElem i xs else x == i