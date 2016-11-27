main :: IO ()
main = do
  m <- readLn :: IO (Int)
  n <- readLn :: IO (Int)
  putStrLn $ show $ solve m n

-- backtracking
solve :: Int -> Int -> Int
solve m n = search m powers where
  powers = reverse $ takeWhile (<= m) $ map (^n) $ [1..]  -- head calls
  search 0 _      = 1  -- edge case: one combination
  search _ []     = 0
  search m (x:xs) = if x > m then search m xs else search (m-x) xs + search m xs
