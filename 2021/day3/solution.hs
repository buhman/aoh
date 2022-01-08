import Data.List (transpose)

data Bit = Zero | One | Inv
  deriving (Show, Eq)

parseBits :: String -> [Bit]
parseBits = map f
  where
    f '0' = Zero
    f '1' = One

parseInput :: String -> [[Bit]]
parseInput = map parseBits . lines

count :: [Bit] -> (Int, Int)
count = foldl f (0, 0)
  where
    f (zeros, ones) Zero = (zeros + 1, ones)
    f (zeros, ones) One = (zeros, ones + 1)

mostCommon :: [Bit] -> Bit
mostCommon = f . count
  where
    f (zeros, ones)
      | zeros > ones = Zero
      | ones > zeros = One

toInt :: [Bit] -> Int
toInt = foldl f 0
  where
    int Zero = 0
    int One = 1
    f acc bit = acc * 2 + (int bit)

part1 :: [[Bit]] -> Int
part1 input = gamma * epsilon
  where
    common = map mostCommon . transpose $ input
    gamma = toInt common
    otherBit Zero = One
    otherBit One = Zero
    epsilon = toInt . map otherBit $ common

rating :: ((Int, Int) -> Bit) -> Int -> [[Bit]] -> Int
rating _ _ (x:[]) = toInt x
rating pred ix input = rating pred (ix + 1) (filter f input)
  where
    ixBit = pred . count . map (!!ix) $ input
    f = (==ixBit) . (!!ix)

part2 :: [[Bit]] -> Int
part2 input = oxygenRating * cooRating
  where
    oxygenRating =
      let pred (zeros, ones)
            | zeros > ones = Zero
            | otherwise    = One
      in rating pred 0 input

    cooRating =
      let pred (zeros, ones)
            | ones < zeros = One
            | otherwise    = Zero
      in rating pred 0 input

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
