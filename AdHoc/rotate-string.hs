import Control.Monad

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    s <- getLine
    let ret = solve s
    putStrLn $ unwords ret


solve :: String -> [String]
solve s = rotate s []

rotate :: String -> [String] -> [String]
rotate s@(x:xs) acc
  | length acc /= length s = rotate cur (acc++[cur])
  | otherwise = acc where
    cur = xs++[x]
