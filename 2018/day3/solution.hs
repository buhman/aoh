import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text.IO as IO
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

number :: Parser Integer
number = read <$> some numberChar

claimNumber :: Parser Integer
claimNumber = char '#' >> number

data Point = Point Integer Integer
  deriving (Show, Eq, Ord)

claimOrigin :: Parser Point
claimOrigin = Point
  <$> number
  <*  char ','
  <*> number

data Area = Area Integer Integer
  deriving (Show)

claimArea :: Parser Area
claimArea = Area
  <$> number
  <*  char 'x'
  <*> number

data Claim = Claim
  { num :: Integer
  , origin :: Point
  , area :: Area
  }
  deriving (Show)

claim :: Parser Claim
claim = Claim
  <$> claimNumber
  <*  string " @ "
  <*> claimOrigin
  <*  string ": "
  <*> claimArea

claims :: Parser [Claim]
claims = many (claim <* char '\n')

parseInput :: Text -> Either (ParseErrorBundle Text Void) [Claim]
parseInput = parse claims ""

type Fabric = Map Point (Set Integer)

claimPoints :: Claim -> [(Integer, Point)]
claimPoints (Claim n (Point x y) (Area w h)) =
  [ (n, Point xi yi)
  | xi <- [x .. (x + w - 1)]
  , yi <- [y .. (y + h - 1)]
  ]

claimsPoints :: [Claim] -> [(Integer, Point)]
claimsPoints = concat . map claimPoints

fabricPoint :: Fabric -> (Integer, Point) -> Fabric
fabricPoint m (n, p) = M.alter f p m
  where
    f Nothing = Just (S.singleton n)
    f (Just s) = Just (S.insert n s)

fabricPoints :: Fabric -> [(Integer, Point)] -> Fabric
fabricPoints = foldl fabricPoint

part1 :: Fabric -> Integer
part1 = M.foldl count 0
  where
    count n s
      | S.size s == 1 = n
      | otherwise = n + 1


overlap :: Fabric -> Set Integer
overlap = S.unions . filter ((>1) . S.size) . M.elems

part2 :: Fabric -> [Claim] -> [Integer]
part2 m cs = [ n
             | (Claim n _ _) <- cs
             , not $ n `S.member` nums
             ]
  where
    nums = overlap m

main :: IO ()
main = do
  Right cs <- parse claims "" <$> IO.readFile "input.txt"
  let fabric = fabricPoints M.empty . claimsPoints $ cs
  putStrLn $ "part1 " <> (show $ part1 fabric)
  putStrLn $ "part2 " <> (show $ part2 fabric cs)
