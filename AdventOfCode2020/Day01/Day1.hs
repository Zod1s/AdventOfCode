import System.IO

main :: IO()
main = do
    input <- (map (\x -> read x :: Int) . lines) <$> readFile "input.txt"
    print $ part1 input

part1 :: [Int] -> Int
part1 ls = foldr foldrFunc 0 [[x, y] | x <- ls, y <- ls]
    where foldrFunc x@(y:z:_) acc = if sum x == 2020
                                    then y*z
                                    else acc

part2 :: [Int] -> Int
part2 ls = foldr foldrFunc 0 [[x, y, z] | x <- ls, y <- ls, z <- ls]
    where foldrFunc x@(y:z:w:_) acc = if sum x == 2020
                                      then y*z*w
                                      else acc