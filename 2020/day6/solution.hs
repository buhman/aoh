import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map

parseInput :: String -> [[[Char]]]
parseInput s = foldl f [[]] . lines $ s
  where
    f (h:t) "" = []:(h:t)
    f (h:t) c = (c:h):t

part1 :: [[[Char]]] -> Int
part1 = sum . map countGroup
  where
    countGroup = Set.size . Set.fromList . concat

part2 :: [[[Char]]] -> Int
part2 = sum . map countGroup
  where
    increment hm k = Map.insertWith (+) k 1 hm
    countGroup g = let size = length g
                       f = length . filter (==size) . Map.elems
                         . foldl increment Map.empty . concat
                   in f g

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ "part1 " <> (show $ part1 input)
  putStrLn $ "part2 " <> (show $ part2 input)
