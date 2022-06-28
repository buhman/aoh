import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Bits as B

-------------------
-- begin scanner --
-------------------

data Token
  = Mem
  | Mask
  | LParen
  | RParen
  | Equal
  | Digits [Char]
  deriving (Show, Eq)

resolveLookBehind :: [Char] -> Maybe Token
resolveLookBehind cs =
  case cs of
    [] -> Nothing
    "mem" -> Just Mem
    "ksam" -> Just Mask
    rcs -> Just $ Digits rcs

concatMaybe :: [a] -> Maybe a -> [a]
concatMaybe xs x =
  case x of
    Nothing -> xs
    Just a -> a:xs

scanToken :: ([Char], [Token]) -> Char -> ([Char], [Token])
scanToken (lookBehind, tokens) c =
  case c of
    '=' -> ([], Equal:newTokens)
    '[' -> ([], LParen:newTokens)
    ']' -> ([], RParen:newTokens)
    ' ' -> ([], newTokens)
    '\n' -> ([], newTokens)
    x -> (x:lookBehind, tokens)
  where
    newTokens = concatMaybe tokens $ resolveLookBehind lookBehind

scanTokens :: T.Text -> [Token]
scanTokens t = reverse $ concatMaybe tokens $ resolveLookBehind lookBehind
  where
    (lookBehind, tokens) = T.foldl scanToken ([], []) t

-----------------
-- end scanner --
-----------------

------------------
-- begin parser --
------------------

data Tri
  = T0
  | T1
  | TZ
  deriving (Show, Eq, Ord)

type Int36 = V.Vector Tri

data Instruction
  = SetMask Int36
  | SetMem Int36 Int36
  deriving (Show, Eq)

{-
grammar

  statement → setMaskStmt
            | setMemStmt

  setMaskStmt → "mask" "=" ( "X" | "0" | "1" ){36}

  setMemStmt → "mem" "[" number "]" "=" number

  number → ( "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" )+
-}

parse :: [Token] -> [Instruction]
parse [] = []
parse (Mask : Equal : Digits m : ts)
  = SetMask (parseMask m) : parse ts
parse (Mem : LParen : Digits a : RParen : Equal : Digits n : ts)
  = SetMem (parseDigits a) (parseDigits n) : parse ts

parseMask :: [Char] -> Int36
parseMask xs = V.unfoldr go xs
  where
    go [] = Nothing
    go (c:cs) = Just $ (
      case c of
        'X' -> TZ
        '0' -> T0
        '1' -> T1
      , cs)

zero :: Int36
zero = V.replicate 36 T0

one :: Int36
one = V.generate 36 (\i -> if i == 0 then T1 else T0)

two :: Int36
two = add one one

three :: Int36
three = add one two

four :: Int36
four = add two two

five :: Int36
five = add two three

six :: Int36
six = add three three

seven :: Int36
seven = add three four

eight :: Int36
eight = add four four

nine :: Int36
nine = add four five

parseDigits :: [Char] -> Int36
parseDigits [] = zero
parseDigits (c:cs) = add (digit c) (mul10 (parseDigits cs))
  where
    digit '0' = zero
    digit '1' = one
    digit '2' = two
    digit '3' = three
    digit '4' = four
    digit '5' = five
    digit '6' = six
    digit '7' = seven
    digit '8' = eight
    digit '9' = nine

mul10 :: Int36 -> Int36
mul10 a = (sll (add (sll a 2) a) 1)

sll :: Int36 -> Int -> Int36
sll n shift = V.unfoldr go 0
  where
    go 36 = Nothing
    go i = Just (ni i, i+1)
    ni i = if i < shift then T0 else n V.! (i-shift)

add :: Int36 -> Int36 -> Int36
add a b = V.unfoldr go (T0, 0)
  where
    go (_, 36) = Nothing
    go (carry, i) = let (carry', result') = addt carry (a V.! i) (b V.! i)
                    in Just (result', (carry', i+1))
    -- carry a b = (carry, result)
    addt T0 T0 T0 = (T0, T0)
    addt T0 T1 T0 = (T0, T1)
    addt T0 T0 T1 = (T0, T1)
    addt T0 T1 T1 = (T1, T0)
    addt T1 T0 T0 = (T0, T1)
    addt T1 T1 T0 = (T1, T0)
    addt T1 T0 T1 = (T1, T0)
    addt T1 T1 T1 = (T1, T1)

----------------
-- end parser --
----------------

-----------------
-- begin part1 --
-----------------

data State = State
  { mask :: Int36
  , mem :: M.Map Int36 Int36
  }
  deriving (Show)

nextState1 :: State -> Instruction -> State
nextState1 state (SetMask m) = State m (mem state)
nextState1 state (SetMem a n) = State (mask state) newMem
  where
    mask' = mask state
    bit i = let mi = (mask' V.! i)
            in if mi == TZ then (n V.! i) else mi
    newValue = const $ Just $ V.generate 36 bit
    newMem = M.alter newValue a (mem state)

---------------
-- end part1 --
---------------

-----------------
-- begin part2 --
-----------------

concatCons :: [[a]] -> [a] -> [[a]]
concatCons aas as = concatMap go as
  where
    go a = map (a:) aas

addresses :: Int36 -> Int36 -> [Int36]
addresses mask address = map V.fromList . bit 0 $ [[]]
  where
    bit :: Int -> [[Tri]] -> [[Tri]]
    bit 36 aas = aas
    bit i aas = bit (i+1) $ concatCons aas $ case mask V.! i of
      T0 -> [address V.! i]
      T1 -> [T1]
      TZ -> [T0, T1]

nextState2 :: State -> Instruction -> State
nextState2 state (SetMask m) = State m (mem state)
nextState2 state (SetMem a n) = State (mask state) newMem
  where
    mask' = mask state
    addresses' = addresses mask' a
    alterMem = M.alter (const (Just n))
    newMem = foldr alterMem (mem state) addresses'

---------------
-- end part2 --
---------------

toInt :: Int36 -> Int
toInt n = go 0
  where
    go 36 = 0
    go i = (B.shift (bitInt (n V.! i)) i) + (go (i + 1))
    bitInt T0 = 0
    bitInt T1 = 1

partN :: (State -> Instruction -> State) -> [Instruction] -> Int
partN nextState = sum . map toInt . M.elems . mem . state
  where
    state = foldl nextState (State zero M.empty)

main :: IO ()
main = do
  input <- parse . scanTokens <$> T.readFile "input.txt"
  let solve = putStrLn . show . (flip partN) input
  solve nextState1
  solve nextState2
