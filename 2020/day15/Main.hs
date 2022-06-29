import Control.Monad.ST (runST, ST)
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector as V
import Control.Monad (forM_)

initState :: Int -> [Int] -> ST s (MV.MVector s Int)
initState length turns = do
  MV.replicate length (-1) >>= insertTurns
  where
    insertTurns :: MV.MVector s Int -> ST s (MV.MVector s Int)
    insertTurns v = do
      forM_ (zip turns [1..]) (uncurry (MV.write v))
      pure v

run :: Int -> (MV.MVector s Int) -> Int -> Int -> ST s Int
run n v last turn = do
  turnSpoken <- MV.read v last
  let nextN = if turnSpoken == (-1) then 0 else (turn - turnSpoken)
      nextTurn = turn + 1

  if nextTurn == n
    then pure nextN
    else MV.write v last turn >> run n v nextN nextTurn

input :: [Int]
input = [2,0,1,9,5,19]

solve :: Int -> Int
solve n = runST $ do
  v <- initState n input
  i <- run n v (last input) (length input)
  pure i

main :: IO ()
main = do
  putStrLn $ show $ solve 2020
  putStrLn $ show $ solve 30000000
