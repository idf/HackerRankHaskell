f :: [Int] -> [Int]
f [] = []
f [a] = []
f (a:b:xs) = b:(f xs)

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
   inputdata <- getContents
   mapM_ (putStrLn. show). f. map read. lines $ inputdata
