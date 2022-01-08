import qualified Data.IntSet as IntSet

sample :: [Int]
sample = [1721, 979, 366, 299, 675, 1456]

combinations :: [Int] -> [(Int, Int)]
combinations xs = [(a, b) | a <- xs, b <- xs]

part1 :: [Int] -> Int
part1 xs = a * b
  where
    pred (a,b) = a + b == 2020
    (a,b):_ = filter pred . combinations $ xs

-- https://en.wikipedia.org/wiki/3SUM
part2 :: [Int] -> Int
part2 xs = a * b * c
  where
    set = IntSet.fromList xs
    pred (a, b) = IntSet.member (2020 - (a + b)) set
    (a,b):_ = filter pred . combinations $ xs
    c = 2020 - (a + b)

parseInput :: String -> [Int]
parseInput = map read . lines

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ "part1 " <> (show $ part1 input)
  putStrLn $ "part2 " <> (show $ part2 input)
