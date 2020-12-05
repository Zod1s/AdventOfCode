import System.IO
import Data.List.Split(splitOn)
import Data.Char(isDigit, isHexDigit)

compFields :: [[(String, String)] -> Maybe String]
compFields = [ lookup "byr"
             , lookup "iyr"
             , lookup "eyr"
             , lookup "hgt"
             , lookup "hcl"
             , lookup "ecl"
             , lookup "pid"]

checkValidity :: [Maybe String -> Bool]
checkValidity = [ checkByr
                , checkIyr
                , checkEyr
                , checkHgt
                , checkHcl
                , checkEcl
                , checkPid]

checkByr :: Maybe String -> Bool
checkByr (Just x) = all isDigit x && length x == 4 && (read x :: Int) >= 1920 && (read x :: Int) <= 2002 

checkIyr :: Maybe String -> Bool
checkIyr (Just x) = all isDigit x && length x == 4 && (read x :: Int) >= 2010 && (read x :: Int) <= 2020

checkEyr :: Maybe String -> Bool
checkEyr (Just x) = all isDigit x && length x == 4 && (read x :: Int) >= 2020 && (read x :: Int) <= 2030

checkHgt :: Maybe String -> Bool
checkHgt (Just x) = length x > 3 && isValid x
  where isValid x = isDig && ((unit == "cm" && x' >= 150 && x' <= 193) || (unit == "in" && x' >= 59 && x' <= 76))
          where unit = last (tail x) : [last x]
                isDig = all isDigit (init (init x))
                x' = (read (init (init x)) :: Int)

checkHcl :: Maybe String -> Bool
checkHcl (Just (x:rest)) = length rest == 6 && x == '#' && all isHexDigit rest

checkEcl :: Maybe String -> Bool
checkEcl (Just x) = length x == 3 && elem x ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

checkPid :: Maybe String -> Bool
checkPid (Just x) = length x == 9 && all isDigit x

main :: IO()
main = do
  input <- (filter (\x -> x /= "") . splitOn "\n\n") <$> readFile "input.txt"
  let input' = map cond2 $ filter (\x -> length x > 6) $ map toLookup input
  print input'

toLookup :: String -> [(String, String)]
toLookup inp = map toTuple inp'
  where inp' = words inp
        toTuple (a:b:c:':':rest) = ([a, b, c], rest)

cond1 :: [(String, String)] -> Bool
cond1 lookupTable = all (/= Nothing) (fmap ($ lookupTable) compFields)

cond2 :: [(String, String)] -> Bool
cond2 lookupTable = all (/= Nothing) valuesFound && check valuesFound
  where valuesFound = (fmap ($ lookupTable) compFields)

check :: [Maybe String] -> Bool
check xs = if length xs == 7
           then and (zipWith id checkValidity xs)
           else and (zipWith id checkValidity (init xs))
