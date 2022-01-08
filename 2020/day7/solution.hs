import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data QtyBag = QtyBag Int (String, String)
  deriving Show

parseLine :: String -> ((String, String), [QtyBag])
parseLine line = ((color1, color2), f contains)
  where
    color1:color2:_:_:contains = words line
    f :: [String] -> [QtyBag]
    f (qty:c1:c2:_:rest) = QtyBag (read qty) (c1, c2) : f rest
    f _ = []

parseInput :: String -> [((String, String), [QtyBag])]
parseInput = map parseLine . lines

unrollBags :: Map (String, String) [QtyBag] -> (String, String) -> Set (String, String)
unrollBags m key = f mempty nested
  where
    nested = m Map.! key
    f :: Set (String, String) -> [QtyBag] -> Set (String, String)
    f s ((QtyBag _ key1):bags)
      | key1 `Set.member` s = f s bags
      | otherwise = let s' = key1 `Set.insert` s
                    in f s' $ bags <> m Map.! key1
    f s _ = s

part1 :: Map (String, String) [QtyBag] -> Int
part1 m = sum . map (count . unrollBags m) . Map.keys $ m
  where
    count s = if ("shiny", "gold") `Set.member` s then 1 else 0

unrollBags2 :: Map (String, String) [QtyBag] -> (String, String) -> Int
unrollBags2 m key = f $ m Map.! key
  where f ((QtyBag qty key1):rest) = qty + qty * (unrollBags2 m key1) + f rest
        f _ = 0

part2 :: Map (String, String) [QtyBag] -> Int
part2 = flip unrollBags2 ("shiny", "gold")

main :: IO ()
main = do
  input <- Map.fromList <$> parseInput <$> readFile "input.txt"
  putStrLn $ "part1 " <> (show $ part1 input)
  putStrLn $ "part2 " <> (show $ part2 input)
