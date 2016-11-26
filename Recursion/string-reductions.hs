main :: IO ()
main = do
  s <- getLine
  putStrLn $ solve s

solve :: String -> String
solve s = foldl (\acc x -> if x `elem` acc then acc else acc++[x]) [] s
