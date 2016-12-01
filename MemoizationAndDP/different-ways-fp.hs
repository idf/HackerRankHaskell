import Data.Functor
import Control.Monad

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    [n, k] <- map read . words <$> getLine :: IO [Int]
    let ret = solve n k
    putStrLn $ show $ ret

solve :: Int -> Int -> Int
solve n k = mem!!n!!k

mem :: [[Int]]
mem = [[count n k| k <- [0..]] | n <- [0..]] where
  count n k
    | k == 0    = 1
    | k == n    = 1
    | otherwise = (mem!!(n-1)!!(k-1) + mem!!(n-1)!!k) `mod` (10^8+7)
