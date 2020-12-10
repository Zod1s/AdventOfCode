import System.IO
import Data.List.Split(splitOn)
import Data.Char(isDigit, isHexDigit)
import Data.List((\\))

compFields :: [[(String, String)] -> Maybe String]
compFields = [ lookup "byr"
             , lookup "iyr"
             , lookup "eyr"
             , lookup "hgt"
             , lookup "hcl"
             , lookup "ecl"
             , lookup "pid"]

checkValidity :: [[(String, String)] -> Bool]
checkValidity = [ checkByr . lookup "byr"
                , checkIyr . lookup "iyr"
                , checkEyr . lookup "eyr"
                , checkHgt . lookup "hgt"
                , checkHcl . lookup "hcl"
                , checkEcl . lookup "ecl"
                , checkPid . lookup "pid"]

checkByr :: Maybe String -> Bool
checkByr (Just x) = all isDigit x && length x == 4 && (read x :: Int) >= 1920 && (read x :: Int) <= 2002 
checkByr _ = False

checkIyr :: Maybe String -> Bool
checkIyr (Just x) = all isDigit x && length x == 4 && (read x :: Int) >= 2010 && (read x :: Int) <= 2020
checkIyr _ = False

checkEyr :: Maybe String -> Bool
checkEyr (Just x) = all isDigit x && length x == 4 && (read x :: Int) >= 2020 && (read x :: Int) <= 2030
checkEyr _ = False

checkHgt :: Maybe String -> Bool
checkHgt (Just x) = length x > 3 && isValid x
  where isValid m = case unit of
                      "cm" -> x' >= 150 && x' <= 193
                      "in" -> x' >= 59 && x' <= 76
                      _ -> False
          where x' = read dim
                (dim, unit) = span isDigit x
checkHgt _ = False

checkHcl :: Maybe String -> Bool
checkHcl (Just (x:rest)) = length rest == 6 && x == '#' && all isHexDigit rest
checkHcl _ = False

checkEcl :: Maybe String -> Bool
checkEcl (Just x) = length x == 3 && elem x ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
checkEcl _ = False

checkPid :: Maybe String -> Bool
checkPid (Just x) = length x == 9 && all isDigit x
checkPid _ = False

main :: IO()
main = do
  input <- (filter (\x -> x /= "") . splitOn "\n\n") <$> readFile "input.txt"
  let input' = filter cond2 $ filter (\x -> length x > 6) $ map toLookup input
  print $ length input'

toLookup :: String -> [(String, String)]
toLookup inp = map toTuple inp'
  where inp' = words inp
        toTuple (a:b:c:':':rest) = ([a, b, c], rest)

cond1 :: [(String, String)] -> Bool
cond1 lookupTable = all (/= Nothing) (fmap ($ lookupTable) compFields)

cond2 :: [(String, String)] -> Bool
cond2 lookupTable = and (fmap ($ lookupTable) checkValidity)
