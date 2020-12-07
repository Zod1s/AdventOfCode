{-# LANGUAGE BangPatterns #-}

import System.IO
import Data.List.Split(splitOn)
import Data.List((\\))

type Orbit = (String, String)

main :: IO()
main = do
  input <- lines <$> readFile "input.txt"
  let orbits = map toOrbit input
      planets = (map snd orbits) \\ ["COM"]
  print $ (+) 1 $ sum $ map (part1 orbits) planets

toOrbit :: String -> Orbit
toOrbit pair = (b, a)
  where (a:b:[]) = splitOn ")" pair

part1 :: [Orbit] -> String -> Int
part1 orbits planet = go orbits planet 0
  where go orbs "COM" !n = n + 1
        go orbs pl !n = go orbs newPl (n + 1)
          where (Just newPl) = lookup pl orbs
