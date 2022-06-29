{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map as M

import Debug.Trace

data Point = Point
  { x :: Int
  , y :: Int
  , z :: Int
  }
  deriving (Show, Eq, Ord)

data Cube = Active | Inactive deriving (Show, Eq)

type Grid = M.Map Point Cube

data BBox = BBox
  { sx :: Int
  , sy :: Int
  , sz :: Int
  , lx :: Int
  , ly :: Int
  , lz :: Int
  }
  deriving (Show)

emptyBBox :: BBox
emptyBBox = BBox maxBound maxBound maxBound minBound minBound minBound

allNeighbors :: Point -> [Point]
allNeighbors Point{..} =
  [ Point (x+dx) (y+dy) (z+dz)
  | dx <- [-1,0,1]
  , dy <- [-1,0,1]
  , dz <- [-1,0,1]
  , dx /= 0 || dy /= 0 || dz /= 0
  ]

find :: Grid -> Point -> Cube
find = flip (M.findWithDefault Inactive)

neighbors :: Grid -> Point -> [Cube]
neighbors g p = map (find g) (allNeighbors p)

parseInput :: IO (Grid, BBox)
parseInput = do
  rows <- zip [0..] . map (zip [0..]) . lines <$> readFile "input.txt"
  pure . (,BBox (-1) (-1) (-1) (length rows) (length (head rows)) 1) . M.fromList $
    [ (Point x y 0, cube c)
    | (y, row) <- rows
    , (x, c) <- row
    ]
  where
    cube '.' = Inactive
    cube '#' = Active

countActive :: [Cube] -> Int
countActive = length . filter (==Active)

nextCube :: Grid -> Point -> Cube
nextCube g p = next $ find g p
  where
    activeCubes = countActive $ neighbors g p

    next Inactive = if activeCubes == 3 then Active else Inactive
    next Active = if activeCubes == 2 || activeCubes == 3 then Active else Inactive

nextBBox :: Point -> Cube -> BBox -> BBox
nextBBox _ Inactive bb = bb
nextBBox Point{..} Active BBox{..} =
  BBox
    (if x <= sx then x - 1 else sx)
    (if y <= sy then y - 1 else sy)
    (if z <= sz then z - 1 else sz)
    (if x >= lx then x + 1 else lx)
    (if y >= ly then y + 1 else ly)
    (if z >= lz then z + 1 else lz)

pointsBBox :: BBox -> [Point]
pointsBBox BBox{..} =
  [ Point x y z
  | x <- [(sx-7)..(lx+7)]
  , y <- [(sy-7)..(ly+7)]
  , z <- [(sz-7)..(lz+7)]
  ]

nextState :: Grid -> (Grid, BBox) -> Point -> (Grid, BBox)
nextState lastGrid (grid, bbox) p = (grid', bbox')
  where
    cube' = nextCube lastGrid p
    bbox' = nextBBox p cube' bbox
    grid' = M.alter (const $ Just cube') p grid

step :: (Grid, BBox) -> (Grid, BBox)
step (lastGrid, bbox) = foldl (nextState lastGrid) (M.empty, emptyBBox) (pointsBBox bbox)


countActives :: Grid -> Int
countActives = M.foldr isActive 0
  where
    isActive :: Cube -> Int -> Int
    isActive Inactive = id
    isActive Active = (+1)

main :: IO ()
main = do
  inputState <- parseInput
  let part1 = countActives $ fst $ foldl (flip (const step)) inputState [1..6]
  putStrLn $ show $ part1
