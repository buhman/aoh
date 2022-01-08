parseInput :: String -> [Int]
parseInput = map read . lines

part1 :: [Int] -> Int
part1 (a:b:xs) = part1(b:xs) + (if a < b then 1 else 0)
part1 _ = 0

windows :: [Int] -> [Int]
windows (a:b:c:xs) = (a + b + c) : windows (b : c : xs)
windows _ = []

part2 :: [Int] -> Int
part2 = part1 . windows

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  let answer1 = part1 input
  print answer1
  let answer2 = part2 input
  print answer2
