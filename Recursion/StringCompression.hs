compress :: String -> String
compress "" = ""
compress xs
  | count xs 1 == 1 = (head xs) : (compress $ tail xs)
  | otherwise = [head xs] ++ (show $ count xs 1) ++ (compress $ drop (count xs 1) xs)

count :: String -> Int -> Int
count "" i = i
count (x:[]) i = i
count (x:xs) i
  | x == head xs = count xs (i+1)
  | otherwise = i

main = do
  xs <- getLine
  putStrLn $ compress xs
