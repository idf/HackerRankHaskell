permute :: String -> String
permute "" = ""
permute xs = (head $ tail xs) : (head xs) : (permute $ tail $ tail xs)

main = do
    t <- readLn :: IO Int
    cases <- getContents
    mapM_ putStrLn $ map permute $ words cases
