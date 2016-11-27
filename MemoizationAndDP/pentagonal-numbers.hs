import Control.Monad

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    x <- readLn
    putStrLn $ show $ solve x

{-
Solve the recurrence equation
-}
solve :: Int -> Int
solve n = 3 * n * (n-1) `quot` 2 + (n-1) + 1  -- '\' resulting float

{-
p(1) = 1 = 1 * 5 - 4
p(2) = 1 * 5
p(3) = 2 * 5 + p(2) - (2 * 2 -1) = 15 - 3 = 12
p(4) = 3 * 5 + p(3) - (3 * 2 -1) = 15 + 12 - 5 = 22
-}
solveTLE :: Int -> Int
solveTLE 1 = 1
solveTLE 2 = 5
solveTLE x = (x-1) * 5 + solveTLE (x-1) - (x-1) * 2 + 1
