import System.IO
import Data.List((\\))
import Data.List.Split(chunksOf)

main :: IO()
main = do
    input <- lines <$> readFile "input.txt"
    print $ part1 input 

part1 :: [String] -> Int
part1 grid = length $ filter (=='#') $ concat $ stabilize grid

stabilize :: [String] -> [String]
stabilize grid = if new == grid then new else stabilize new
    where new = nextStep grid

nextStep :: [String] -> [String]
nextStep grid = chunksOf w $ (map update) grid' 
    where grid' = [b | x <- [0..w-1], y <- [0..h-1], 
                   let b = toUpdate grid (x, y)]
          w = length $ grid !! 0
          h = length grid

neighbours :: (Int, Int) -> Int -> Int -> [(Int, Int)]
neighbours (x, y) w h = [(x', y') | x' <- [x-1, x, x+1], y' <- [y-1, y, y+1],
                          y' >= 0, x' >= 0, y' < h, x' < w] \\ [(x,y)] 

update :: (Char, Bool) -> Char
update ('L', True) = '#'
update ('#', True) = 'L'
update (c, False) = c

toUpdate :: [String] -> (Int, Int) -> (Char, Bool)
toUpdate grid pos@(x, y) = case c of
    '.' -> (c, False)
    '#' -> (c, hashCond grid neigh)
    'L' -> (c, lCond grid neigh)
    where c = (grid !! y) !! x
          w = length $ grid !! 0
          h = length grid
          neigh = neighbours pos w h

hashCond :: [String] -> [(Int, Int)] -> Bool
hashCond grid neigh = (length $ filter (=='#') chars) >= 4
    where chars = map (\(x, y) -> (grid !! y) !! x) neigh

lCond :: [String] -> [(Int, Int)] -> Bool
lCond grid neigh = (length $ filter (=='#') chars) == 0
    where chars = map (\(x, y) -> (grid !! y) !! x) neigh