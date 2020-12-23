{-# LANGUAGE BangPatterns #-}
import System.IO

main :: IO()
main = do
    input <- lines <$> readFile "input.txt"
    print $ part1 input

part1 :: [String] -> Int
part1 = slopeChecker 3 1

slopeChecker :: Int -> Int -> [String] -> Int
slopeChecker r d strl = go 0 0 strl
    where go !i acc [] = acc
          go !i acc (x:xs) = if (c == '#')
                             then go (i + 1) (acc + 1) xs
                             else go (i + 1) acc xs
              where c = head $ drop (r * i) (concat $ repeat x)

slopes :: [(Int, Int)]
slopes = [(1, 1), (3, 1), (5, 1), (7, 1)]

part2 :: [String] -> Int
part2 strl = (*) n $ product $ map (\(x, y) -> slopeChecker x y strl) slopes
    where n = go 0 0 strl
          go !i acc [] = acc
          go !i acc [_] = acc
          go !i acc (x:_:xs) = if (c == '#')
                               then go (i + 1) (acc + 1) xs
                               else go (i + 1) acc xs
              where c = head $ drop i (concat $ repeat x)