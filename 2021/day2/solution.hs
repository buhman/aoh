data Instruction
  = Forward Int
  | Down Int
  | Up Int
  deriving Show

data State = State
  { position :: Int
  , depth :: Int
  , aim :: Int
  }
  deriving Show

parseInstruction :: String -> Instruction
parseInstruction line | direction == "forward" = Forward magnitude'
                      | direction == "down"    = Down magnitude'
                      | direction == "up"      = Up magnitude'
  where
    (direction:magnitude:[]) = words line
    magnitude' = read magnitude

parseInput :: String -> [Instruction]
parseInput = map parseInstruction . lines

step :: State -> Instruction -> State
step state (Forward n) = state { position = (position state) + n }
step state (Down n)    = state { depth = (depth state) + n }
step state (Up n)      = state { depth = (depth state) - n }

solve :: State -> [Instruction] -> State
solve state [] = state
solve state (instruction:remaining) = solve nextState remaining
  where
    nextState = step state instruction

main :: IO ()
main = do
  instructions <- parseInput <$> readFile "input.txt"
  let part1 = solve (State 0 0 0) instructions
  print $ (position part1 * depth part1)
