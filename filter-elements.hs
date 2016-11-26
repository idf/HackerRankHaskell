{-
Fold with Map
-}
import Data.Functor  -- <$>
import Control.Monad
import Data.List -- nub
import qualified Data.Map as M

type Table = M.Map Int Int

main :: IO ()
main = do
  t <- readLn :: IO (Int)
  replicateM_ t $ do
    k <- last . map (read :: String -> Int) . words <$> getLine  -- <$> is liftM
    xs <- map (read :: String -> Int) . words <$> getLine
    let ret = solve xs k
    let msg = if null ret then "-1" else intercalate " " $ map show ret
    putStrLn msg

solve :: [Int] -> Int -> [Int]
solve xs k = ret where
  m = foldl itr M.empty xs
  ret = filter (predicate m k) $ nub xs
  itr m x =
    case M.lookup x m of
      Nothing -> M.insert x 1 m
      Just n  -> M.insert x (n+1) m
  predicate m k x =
    case M.lookup x m of
      Nothing -> False
      Just n  -> n >= k
