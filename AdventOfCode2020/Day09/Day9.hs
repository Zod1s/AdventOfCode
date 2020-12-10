import System.IO

main :: IO()
main = do
  input <- (map (\x -> (read x :: Integer)) . lines) <$> readFile "input.txt"
  let toMatch = part1 input
  print $ part2 toMatch input
  
part1 :: [Integer] -> Integer
part1 list = if all (/= toMatch) [x + y | x <- available, y <- available]
             then toMatch
             else part1 (tail list)
  where available = take 25 list
        toMatch = head $ drop 25 list

part2 :: Integer -> [Integer] -> Integer
part2 toMatch list = (minimum ns) + (maximum ns)
  where ns = findGroup toMatch list

findGroup :: Integer -> [Integer] -> [Integer]
findGroup toMatch list = go [] toMatch list (tail list)
  where m = minimum list
        list' = filter (< toMatch - m) list
        go l m [] _ = if sum l == m then l else []
        go l m (x:xs) ls = if sum l == m
                           then l
                           else if sum l < m
                                then go (x:l) m xs ls
                                else go [] m ls (tail ls)
