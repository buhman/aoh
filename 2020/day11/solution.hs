import qualified Data.Vector as V
import Debug.Trace (traceShowId)

data Grid a = Grid
  { gridVector :: V.Vector a
  , gridWidth :: Int
  , gridHeight :: Int
  }

data Point = Point Int Int
  deriving Eq

(!) :: Grid a -> Point -> a
g ! p = gridVector g V.! asIndex g p

asIndex :: Grid a -> Point -> Int
asIndex g (Point x y) = y * gridWidth g + x

adjacent :: Point -> [Point]
adjacent (Point x y)
  = Point (x-1) (y-1)
  : Point (x+0) (y-1)
  : Point (x+1) (y-1)
  : Point (x-1) (y+0)

  : Point (x+1) (y+0)
  : Point (x-1) (y+1)
  : Point (x+0) (y+1)
  : Point (x+1) (y+1)
  : []

pointInGrid :: Grid a -> Point -> Bool
pointInGrid g (Point x y) = x >= 0 && x < gridWidth g
                         && y >= 0 && y < gridHeight g

data Seat = Floor | Empty | Occupied
  deriving Eq

instance Show Seat where
  show Floor = "."
  show Empty = "L"
  show Occupied = "#"

parseSeat :: Char -> Seat
parseSeat '.' = Floor
parseSeat 'L' = Empty
parseSeat '#' = Occupied

parseInput :: String -> Grid Seat
parseInput s = Grid (seats rows) width height
  where
    rows = lines s
    width = length $ head rows
    height = length rows
    parseRow = map parseSeat
    seats = V.fromList . concat . map parseRow

instance Show a => Show (Grid a) where
  show g = unlines [ concat [ show $ g ! (Point x y)
                            | x <- [0 .. gridWidth g - 1]
                            ]
                   | y <- [0 .. gridHeight g - 1]
                   ]

rule1 :: Grid Seat -> Point -> Seat -> Seat
rule1 g p seat = case seat of
              Empty    -> if occupied == 0 then Occupied else seat
              Occupied -> if occupied >= 4 then Empty    else seat
              _        -> seat
  where
    occupied = length
             . filter ((== Occupied) . (g !))
             . filter (pointInGrid g)
             . adjacent
             $ p

rule1i :: Grid Seat -> Int -> Seat -> Seat
rule1i g i = rule1 g (Point x y)
  where (y, x) = divMod i (gridWidth g)

step1 :: Grid Seat -> Grid Seat
step1 g = g { gridVector = V.imap (rule1i g) (gridVector g) }

part1 :: Grid Seat -> Int
part1 last = let next = step1 last
             in if (gridVector next) == (gridVector last)
                then occupied $ gridVector next
                else part1 next
  where
    occupied = length . V.filter (== Occupied)


main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ "part1 " <> (show $ part1 input)
