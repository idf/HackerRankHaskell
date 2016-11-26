compress :: String -> String -> IO()
compress xs ys = do
  let pre = prefix xs ys
  let len = length pre
  putStrLn $ (show len) ++ " " ++ pre
  let xs' = drop len xs
  let ys' = drop len ys
  putStrLn $ (show $ length xs') ++ " " ++ xs'
  putStrLn $ (show $ length ys') ++ " " ++ ys'

prefix :: String -> String -> String
prefix (x:xs) (y:ys)
  | x == y    = x:(prefix xs ys)
  | otherwise = ""
prefix _ _    = ""

main = do
  xs <- getLine
  ys <- getLine
  compress xs ys
