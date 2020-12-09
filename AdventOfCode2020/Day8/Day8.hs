{-# LANGUAGE BangPatterns #-}

import System.IO()
import Data.List.Split(splitOn)
import Data.List(sort)

data Instruction = NOP Int
                 | ACC Int
                 | JMP Int

instance Show Instruction where
  show (NOP _) = "nop"
  show (ACC a) = "acc " ++ (show a)
  show (JMP a) = "jmp " ++ (show a) 

instance Read Instruction where
  readsPrec _  input =
    let instr = case input of
                  ('n':'o':'p':' ':rest) -> NOP $ readSign rest
                  ('a':'c':'c':' ':rest) -> ACC $ readSign rest
                  ('j':'m':'p':' ':rest) -> JMP $ readSign rest
    in (\p -> [(instr, "")]) input

readSign :: String -> Int
readSign ('+':rest) = read rest
readSign rest = read rest

instance Eq Instruction where
  (NOP _) == (NOP _) = True
  (JMP _) == (JMP _) = True
  (ACC _) == (ACC _) = True
  _ == _ = False

main :: IO()
main = do
  input <- (toCode . lines) <$> readFile "input.txt"
  print $ part2 input

toCode :: [String] -> [Instruction]
toCode = map read

part1 :: [Instruction] -> Int
part1 code = go code 0 0 []
  where go c !acc !i !inds = if (elem i inds) || (i >= length c)
                             then acc
                             else case instr of
                                   NOP _   -> go c acc (i + 1) (sort (i:inds))
                                   ACC arg -> go c (acc + arg) (i + 1) (sort (i:inds))
                                   JMP off -> go c acc (i + off) (sort (i:inds))
          where instr = c !! i

halts :: [Instruction] -> Maybe Int
halts code = go code 0 0 []
  where go c !acc !i !inds = if (elem i inds)
                             then Nothing
                             else if i >= length c
                                  then Just acc
                                  else case instr of
                                         NOP _   -> go c acc (i + 1) (sort (i:inds))
                                         ACC arg -> go c (arg + acc) (i + 1) (sort (i:inds))
                                         JMP off -> go c acc (i + off) (sort (i:inds))
          where instr = c !! i

part2 :: [Instruction] -> Int
part2 code = halted
  where modif = modify code
        (Just halted) = head $ filter (\x -> x /= Nothing) $ map halts modif

modify :: [Instruction] -> [[Instruction]]
modify code = [h ++ [swap c] ++ tt | i <- idxs, let h = take i code
                                                      , let t = drop i code
                                                              , let c = head t, let tt = tail t]
  where idxs = go 0 code
        go !i [] = []
        go !i (c:cs) = if c == (NOP 0) || c == (JMP 0)
                       then i : (go (i + 1) cs)
                       else (go (i + 1) cs)
        swap (NOP x) = JMP x
        swap (JMP x) = NOP x
