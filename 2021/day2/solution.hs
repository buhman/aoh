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

part1 :: [Instruction] -> Int
part1 instructions = position state * depth state
  where
    state = solve (State 0 0 0) instructions

step2 :: State -> Instruction -> State
step2 state (Down n) = state { aim = (aim state) + n }
step2 state (Up n) = state { aim = (aim state) - n }
step2 state (Forward n) = state { position = position', depth = depth' }
  where position' = (position state) + n
        depth' = (depth state) + (aim state) * n

part2 :: [Instruction] -> Int
part2 = answer . foldl step2 (State 0 0 0)
  where
    answer state = position state * depth state

main :: IO ()
main = do
  instructions <- parseInput <$> readFile "input.txt"
  putStrLn $ "part1 " <> (show $ part1 instructions)
  putStrLn $ "part2 " <> (show $ part2 instructions)
