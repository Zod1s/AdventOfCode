import System.IO

main :: IO()
main = do
  input <- lines <$> readFile "input.txt"
  
