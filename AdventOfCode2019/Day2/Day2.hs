{-# LANGUAGE BangPatterns #-}

import System.IO

main :: IO()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let text = map (\x -> (read x :: Int)) (lines contents)
  print $ part2 text

part1 :: [Int] -> Int
part1 comms = head (eval 0 cmds)
  where cmds = (head comms) : [12] ++ [2] ++ (drop 3 comms)

part2 :: [Int] -> Int
part2 comms = 100 * n + v
  where (n, v) = head $ filter (\(x, y) -> eval' x y == 19690720) [(x, y) | x <- [0..99], y <- [0..99]]
        eval' x y = head $ eval 0 cmds
          where cmds = (head comms) : [x] ++ [y] ++ (drop 3 comms)

eval :: Int -> [Int] -> [Int]
eval !i xs
  | c == 99 = xs
  | c == 1 = eval (i + 4) ((take z xs) ++ [s] ++ (drop (1 + z) xs))
  | c == 2 = eval (i + 4) ((take z xs) ++ [p] ++ (drop (1 + z) xs))
  | otherwise = [-1]
  where (c:x:y:z:[]) = take 4 (drop i xs)
        x' = xs !! x
        y' = xs !! y
        s = x' + y'
        p = x' * y'
