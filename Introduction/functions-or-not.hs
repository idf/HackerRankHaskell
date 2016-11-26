import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    n <- readLn
    input <- replicateM n getLine
    ret <- solve input
    putStrLn ret

-- one-to-one mapping
solve :: [String] -> IO (String)
solve input = do
  let xs = fmap (head . map read . words) input :: [Int]
  let ret = if xs == nub xs then "YES" else "NO"
  return ret
