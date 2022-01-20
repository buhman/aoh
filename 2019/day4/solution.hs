input :: [Int]
input = [246540 .. 787419]

digits :: Int -> [Int]
digits n = go n 5
  where
    go n' i
      | i < 0 = []
      | otherwise =
        let (d, m) = n' `divMod` (10 ^ i)
        in d : go m (i - 1)

hasAdjacent :: [Int] -> Bool
hasAdjacent (a:b:rest) = a == b || hasAdjacent (b:rest)
hasAdjacent _ = False

hasAdjacent2 :: [Int] -> Bool
hasAdjacent2 (a:b:c:d:e:f:[]) =
     ( a == b && b /= c )
  || ( b == c && a /= b && c /= d )
  || ( c == d && b /= c && d /= e )
  || ( d == e && c /= d && e /= f )
  || ( e == f && d /= e )

isAscending :: [Int] -> Bool
isAscending (a:b:rest) = a <= b && isAscending (b:rest)
isAscending _ = True

part1 :: [Int] -> Int
part1 = length . filter hasAdjacent . filter isAscending . map digits

part2 :: [Int] -> Int
part2 = length . filter hasAdjacent2 . filter isAscending . map digits
