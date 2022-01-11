import qualified Data.Vector as V

data Grid a = Grid
  { gridVector :: V.Vector a
  , gridWidth :: Int
  , gridHeight :: Int
  }

data Point = Point Int Int
  deriving (Eq, Show)

(!) :: Grid a -> Point -> a
g ! p = gridVector g V.! asIndex g p

asIndex :: Grid a -> Point -> Int
asIndex g (Point x y) = y * gridWidth g + x

fromIndex :: Grid a -> Int -> Point
fromIndex g i = Point x y
  where
    (y, x) = divMod i (gridWidth g)

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

rule1 :: Grid Seat -> Int -> Seat -> Seat
rule1 g i seat = case seat of
                   Empty    -> if occupied == 0 then Occupied else seat
                   Occupied -> if occupied >= 4 then Empty    else seat
                   _        -> seat
  where
    occupied = length
             . filter ((== Occupied) . (g !))
             . filter (pointInGrid g)
             . adjacent
             $ fromIndex g i

type Rule = Grid Seat -> Int -> Seat -> Seat

step :: Rule -> Grid Seat -> Grid Seat
step rule g = g { gridVector = V.imap (rule g) (gridVector g) }

solve :: Rule -> Grid Seat -> Int
solve rule last = let next = step rule last
                  in if (gridVector next) == (gridVector last)
                     then occupied $ gridVector next
                     else solve rule next
  where
    occupied = length . V.filter (== Occupied)

first :: [a] -> [a]
first (x:_) = [x]
first [] = []

castRays :: (Seat -> Bool) -> Grid Seat -> Point -> [Point]
castRays pred g p = points
  where
    rays :: [ ((Int -> Int), (Int -> Int)) ]
    rays = let s = subtract
           in [ ((s 1), (s 1))
              , ((+ 0), (s 1))
              , ((+ 1), (s 1))
              , ((s 1), (+ 0))

              , ((+ 1), (+ 0))
              , ((s 1), (+ 1))
              , ((+ 0), (+ 1))
              , ((+ 1), (+ 1))
              ]

    addRay (Point x y) (fx, fy) = Point (fx x) (fy y)

    rayPoints point ray = let p = (addRay point ray)
                          in p:(rayPoints p ray)

    points = [ pt
             | ray <- rays
             , pt <- first . filter (pred . (g !)) . takeWhileOne ((==Floor) . (g !))
                                                   . takeWhile (pointInGrid g)
                                                   . rayPoints p
                                                   $ ray
             ]

takeWhileOne :: (a -> Bool) -> [a] -> [a]
takeWhileOne pred [] = []
takeWhileOne pred (x:xs) = if pred x then x : takeWhileOne pred xs else [x]

rule2 :: Grid Seat -> Int -> Seat -> Seat
rule2 g i seat = case seat of
                   Empty    -> if occupied == 0 then Occupied else seat
                   Occupied -> if occupied >= 5 then Empty    else seat
                   _        -> seat
  where
    occupied = length
             . castRays (==Occupied) g
             $ fromIndex g i

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  let part1 = solve rule1
      part2 = solve rule2
  putStrLn $ "part1 " <> (show $ part1 input)
  putStrLn $ "part2 " <> (show $ part2 input)
