{-# LANGUAGE BangPatterns #-}

import Data.List(elemIndex)

input :: [Int]
input = reverse [16, 11, 15, 0, 1, 7]

part1 :: [Int] -> Int
part1 l = go 2020 l (length l)

part2 :: [Int] -> Int
part2 l = go 30000000 l (length l)

go :: Int -> [Int] -> Int -> Int
go m l !i = if m == i
            then head l
            else case index of
                   Nothing -> go m (0 : l) (i + 1)
                   Just ii -> go m ((1 + ii) : l) (i + 1)
  where lastN = head l
        index = elemIndex lastN (tail l)

main :: IO()
main = print $ part2 input
