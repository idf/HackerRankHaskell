import Data.Functor
import Data.Char

main :: IO ()
main = do
  [n, k] <- words <$> getLine
  let ret = solve n k
  putStr $ ret

solve :: String -> String -> String
solve n k = superDigit $ show $ (read k) * (read $ digitSum n)

digitSum :: String -> String
digitSum x = show $ sum $ map digitToInt x

superDigit :: String -> String
superDigit x
  | length x == 1 = x
  | otherwise = superDigit $ digitSum x

-- digital root, overflow
solveError :: Int -> Int -> Int
solveError n k = ret where
  ret = digitalRoot $ k * n
  digitalRoot x
    | x == 0 = 0
    | x `mod` 9 == 0 = 9
    | otherwise = x `mod` 9
