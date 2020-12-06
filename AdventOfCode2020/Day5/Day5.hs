import System.IO
import Data.List(sort)

main :: IO()
main = do
  inputs <- lines <$> readFile "input.txt"
  print $ missing $ map convert inputs

convert :: String -> Int
convert (a:b:c:d:e:f:g:h:i:j:[]) = if valid
                                   then convert' 'F' [a, b, c, d, e, f, g] * 8 + convert' 'L' [h, i, j]
                                   else -1
  where valid = all (`elem` "FB") [a, b, c, d, e, f, g] && all (`elem` "LR") [h, i, j] 
convert _ = -1

convert' :: Char -> String -> Int
convert' ctrl str = foldl (\acc x -> x + 2 * acc) 0 $ map (\x -> if x == ctrl then 0 else 1) str

missing :: [Int] -> Int
missing list = head [x | x <- [m1..m2], not (elem x list')]
  where m1 = head list'
        m2 = last list'
        list' = sort list
