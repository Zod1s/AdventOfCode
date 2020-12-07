-- part 2 doesn't work
import Data.Char(digitToInt)

lower :: Int
lower = 265275

upper :: Int
upper = 781584

solutions :: [String]
solutions = filter cond2 [show x | x <- [lower..upper]]

cond1 :: String -> Bool
cond1 (a:b:c:d:e:f:[]) = any (==0) l && all (>=0) l
  where a' = digitToInt a
        b' = digitToInt b
        c' = digitToInt c
        d' = digitToInt d
        e' = digitToInt e
        f' = digitToInt f
        l = [b'-a', c'-b', d'-c', e'-d', f'-e']

cond2 :: String -> Bool
cond2 (a:b:c:d:e:f:[]) = any (==0) l && all (>=0) l && rec l
  where a' = digitToInt a
        b' = digitToInt b
        c' = digitToInt c
        d' = digitToInt d
        e' = digitToInt e
        f' = digitToInt f
        l = [b'-a', c'-b', d'-c', e'-d', f'-e']

rec :: [Int] -> Bool
rec [] = True
rec [x] = True 
rec [x,y] = if x == 0 then x /= y else True
rec (x:yy@(y:zz@(z:rest))) = if x == 0 && y == 0
                        then z/= 0 && rec yy
                        else if x == 0
                             then rec zz
                             else rec yy
      
main :: IO()
main = print $ length solutions
