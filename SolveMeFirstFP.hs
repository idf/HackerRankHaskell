solveMeFirst a b = a + b

main = do
    a <- readLn
    b <- readLn
    let sum = solveMeFirst a b
    print sum
