import System.IO
import Data.Angle(Degrees(..), sine, cosine)

data Complex = !Int :+ !Int
             deriving (Show, Eq)

realPart, imagPart :: Complex -> Int
realPart (c :+ _) = c
imagPart (_ :+ c) = c

cis :: Degrees Double -> Complex
cis angle = c :+ s
  where c = round $ cosine angle
        s = round $ sine angle

instance Num Complex where
  (a :+ b) + (x :+ y) = ((a + x) :+ (b + y))
  (a :+ b) - (x :+ y) = ((a - x) :+ (b - y))
  (a :+ b) * (x :+ y) = ((a * x - b * y) :+ (a * y + b * x))
  signum _ = undefined
  abs _ = undefined
  fromInteger _ = undefined

data Ship = Sh Complex Complex
          deriving(Show)

data Command = M Complex
             | R Complex
             | L Complex
             | F Complex
             deriving(Show, Eq)

instance Read Command where
  readsPrec _ input =
    let instr = case input of
                  ('N':rest) -> M $ (0 :+ (read rest))
                  ('S':rest) -> M $ (0 :+ (negate $ read rest))
                  ('E':rest) -> M $ ((read rest) :+ 0)
                  ('W':rest) -> M $ ((negate $ read rest) :+ 0)
                  ('R':rest) -> R $ rotation (-1) rest
                  ('L':rest) -> L $ rotation   1  rest
                  ('F':rest) -> F $ moveF rest
    in (\p -> [(instr, "")]) input

rotation :: Int -> String -> Complex
rotation d str = cis a
  where amt = read str :: Int
        a = Degrees (fromIntegral (d * amt))

moveF :: String -> Complex
moveF str = amt :+ 0
  where amt = read str :: Int

manhattan :: Complex -> Int
manhattan (x :+ y) = (abs x) + (abs y)

main :: IO()
main = do
  input <- (map (\x -> read x :: Command) . lines) <$> readFile "input.txt"
  print $ part2 input

part1 :: [Command] -> Int
part1 cmds = manhattan (eval1 cmds ship)
  where ship = Sh (1 :+ 0) (0 :+ 0)

eval1 :: [Command] -> Ship -> Complex
eval1 [] (Sh _ pos) = pos
eval1 ((F amt):cmds) (Sh dir pos) = eval1 cmds ship'
  where ship' = Sh dir (pos + dir * amt)
eval1 ((R rot):cmds) (Sh dir pos) = eval1 cmds ship'
  where ship' = Sh (dir * rot) pos
eval1 ((L rot):cmds) (Sh dir pos) = eval1 cmds ship'
  where ship' = Sh (dir * rot) pos
eval1 ((M mov):cmds) (Sh dir pos) = eval1 cmds ship'
  where ship' = Sh dir (mov + pos)


part2 :: [Command] -> Int
part2 cmds = manhattan (eval2 cmds ship)
  where ship = Sh (10 :+ 1) (0 :+ 0)

eval2 :: [Command] -> Ship -> Complex
eval2 [] (Sh _ pos) = pos
eval2 ((F amt):cmds) (Sh way pos) = eval2 cmds ship'
  where ship' = Sh way (pos + way * amt)
eval2 ((R rot):cmds) (Sh way pos) = eval2 cmds ship'
  where ship' = Sh (way * rot) pos
eval2 ((L rot):cmds) (Sh way pos) = eval2 cmds ship'
  where ship' = Sh (way * rot) pos
eval2 ((M mov):cmds) (Sh way pos) = eval2 cmds ship'
  where ship' = Sh (mov + way) pos
