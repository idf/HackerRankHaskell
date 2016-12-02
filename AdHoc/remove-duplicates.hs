main :: IO ()
main = do
  s <- getLine
  let ret = solve s
  putStrLn ret

solve :: String -> String
solve s = foldl (\acc x -> if x `elem` acc then acc else acc++[x]) [] s
