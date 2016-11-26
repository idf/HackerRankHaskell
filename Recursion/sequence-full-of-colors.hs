import Control.Monad

main :: IO ()
main = do
  t <- readLn :: IO (Int) -- replace getLine
  replicateM_ t $ do
    s <- getLine
    putStrLn $ show $ solve s

solve :: String -> Bool
solve s = check 0 0 0 0 s where
  check r g y b [] = r == g && y == b
  check r g y b (x:xs)
    | x == 'R' = abs (r + 1 - g) <= 1 && check (r+1) g y b xs
    | x == 'G' = abs (g + 1 - r) <= 1 && check r (g+1) y b xs
    | x == 'Y' = abs (y + 1 - b) <= 1 && check r g (y+1) b xs
    | x == 'B' = abs (b + 1 - y) <= 1 && check r g y (b+1) xs
