import Control.Monad

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    idx <- readLn
    let ret = solve idx
    putStrLn $ show $ ret

solve :: Int -> Int
solve idx = fibMod !! idx

fib :: [Integer]  -- type Integer is arbitrary precision
fib = 0 : 1 : zipWith (+) fib (tail fib)

fibMod :: [Int]
fibMod = 0 : 1 : zipWith (\a b -> (a+b) `mod` 100000007 ) fibMod (tail fibMod)
