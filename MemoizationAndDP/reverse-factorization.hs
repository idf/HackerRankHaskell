import Data.Maybe
import Data.Functor
import Data.List
import Control.Monad

main :: IO ()
main = do
  (n:_) <- map read . words <$> getLine :: IO [Int]
  xs    <- map read . words <$> getLine :: IO [Int]
  let ret = solve n xs
  putStrLn $ unwords $ map show ret

solve :: Int -> [Int] -> [Int]
solve n xs = fromMaybe [-1] $ search n sortedxs where
  sortedxs = sortBy (flip compare) xs
  search :: Int -> [Int] -> Maybe[Int]
  search 1 _  = Just [1]
  search _ [] = Nothing
  search n a@(x:xs)
    | r == 0    = (++[n]) <$> search q a
    | otherwise = search n xs where
      (q, r) = n `quotRem` x
