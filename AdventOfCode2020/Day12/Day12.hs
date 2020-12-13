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

data Ship = Sh Dir Pos
          deriving(Show)

data Command = M Complex
             | R Complex
             | L Complex
             | F Complex
             deriving(Show)

instance Eq Command where
  (M _) == (M _) = True
  (R _) == (R _) = True
  (L _) == (L _) = True
  (F _) == (F _) = True
  _ == _ = False

instance Read Command where
  readsPrec _ input =
    let instr = case input of
                  ('N':rest) -> M $ ((read rest) :+ 0)
                  ('S':rest) -> M $ ((negate $ read rest) :+ 0)
                  ('W':rest) -> M $ (0 :+ (negate $ read rest))
                  ('E':rest) -> M $ (0 :+ (read rest))
                  ('R':rest) -> R $ rotationR rest
                  ('L':rest) -> L $ rotationR rest
                  ('F':rest) -> F $ moveF rest
    in (\p -> [(instr, "")]) input

type Dir = Complex

type Pos = Complex

rotationR, rotationL, moveF :: String -> Complex
rotationR str = cis a
  where amt = read str :: Int
        a = Degrees (fromIntegral (-amt))
rotationL str = cis a
  where amt = read str :: Int
        a = Degrees (fromIntegral amt)
moveF str = amt :+ 0
  where amt = read str :: Int

main :: IO()
main = do
  input <- (map (\x -> read x :: Command) . lines) <$> readFile "test.txt"
  print $ part1 input

part1 :: [Command] -> Int
part1 cmds = manhattan (eval cmds ship)
  where ship = Sh (0 :+ 1) (0 :+ 0)
        manhattan (x :+ y) = (abs x) + (abs y)

eval :: [Command] -> Ship -> Pos
eval [] (Sh _ pos) = pos
eval ((F amt):cmds) (Sh dir pos) = eval cmds ship'
  where ship' = Sh dir (pos + dir * amt)
eval ((R rot):cmds) (Sh dir pos) = eval cmds ship'
  where ship' = Sh (dir * rot) pos
eval ((L rot):cmds) (Sh dir pos) = eval cmds ship'
  where ship' = Sh (dir * rot) pos
eval ((M mov):cmds) (Sh dir pos) = eval cmds ship'
  where ship' = Sh dir (mov + pos)
