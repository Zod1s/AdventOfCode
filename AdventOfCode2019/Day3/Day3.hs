{- Doesn't work -}

import System.IO
import Data.List.Split (splitOn)
import Data.List (nub, intersect)

data Direction = R
               | L
               | U
               | D
               deriving (Eq, Show, Read)

data Movement = M
  { d :: !Direction
  , l :: !Int}
  deriving (Eq, Show)

data Coord = C !Int !Int
           deriving (Eq, Show)

main :: IO()
main = do
  input <- (map (map convMov . (splitOn ",")) . lines) <$> readFile "input.txt"
  let fs = nub $ concat $ scanl (movs . head) [(C 0 0)] (input !! 0)
      sn = nub $ concat $ scanl (movs . head) [(C 0 0)] (input !! 1)
  print $ minimum (map (\(C x y) -> (abs x) + (abs y)) $ intersect fs sn)

convMov :: String -> Movement
convMov (d:rest) = M (read [d] :: Direction) (read rest :: Int)

addCoord :: Coord -> Coord -> Coord
addCoord (C x y) (C x' y') = C (x+x') (y+y')

convertDir :: Direction -> Coord
convertDir U = C 0 1
convertDir D = C 0 (-1)
convertDir R = C 1 0
convertDir L = C (-1) 0

scale :: Coord -> Int -> Coord
scale (C x y) l = C (l*x) (l*y)

movs :: Coord -> Movement -> [Coord]
movs c@(C x y) (M d l) = [addCoord c (scale dir l')| l' <-[0..l]]
  where dir = convertDir d
