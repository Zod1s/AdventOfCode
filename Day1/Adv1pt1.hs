import System.IO

main :: IO()
main = do
  handle <- openFile "input.txt" ReadMode 
  contents <- hGetContents handle
  let nums = map f (lines contents)
      f n = read n :: Int
  print $ findNums2 nums


findNums2 :: [Int] -> Int
findNums2 ls = foldr foldrFunc 0 [[x, y] | x <- ls, y <- ls]
  where foldrFunc x@(y:z:_) acc = if sum x == 2020
                                    then y*z
                                    else acc

findNums3 :: [Int] -> Int
findNums3 ls = foldr foldrFunc 0 [[x, y, z] | x <- ls, y <- ls, z <- ls]
  where foldrFunc x@(y:z:w:_) acc = if sum x == 2020
                                    then y*z*w
                                    else acc
