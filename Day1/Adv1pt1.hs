import qualified Data.List as L
import System.IO

main :: IO()
main = do
  handle <- openFile "input.txt" ReadMode 
  contents <- hGetContents handle
  let nums = map f (lines contents)
      f n = read n :: Int
  print $ findNums nums

findNums :: [Int] -> Int
findNums ls = foldr foldrFunc 0 [[x, y, z] | x <- ls, y <- ls, z <- ls]
  where foldrFunc x@(y:z:w:_) acc = if sum x == 2020
                                    then y*z*w
                                    else acc
