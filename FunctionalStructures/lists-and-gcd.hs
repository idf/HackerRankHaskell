import Data.Functor
import Control.Monad

main :: IO ()
main = do
  n <- readLn
  xs <- replicateM n $ map (read:: String -> Int) . words <$> getLine
  let ret = solve xs
  putStrLn $ unwords $ map show ret

-- merge list of lists
solve :: [[Int]] -> [Int]
solve xs = foldl1 combine xs where
  combine acc x = comb acc x []
  comb [] _ ret = ret
  comb _ [] ret = ret
  comb lst0@(a:b:xs) lst1@(m:n:ys) ret
    | a == m    = comb xs ys $ ret ++ [a, min b n]
    | a <  m    = comb xs lst1 ret
    | otherwise = comb lst0 ys ret
