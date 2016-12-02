import Control.Monad
import Data.List
import Data.Functor

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    _ <- getLine
    passwords <- words <$> getLine
    guess <- getLine
    case solve passwords guess of
      Nothing -> putStrLn "WRONG PASSWORD"
      Just xs -> putStrLn $ unwords xs

solve :: [String] -> String -> Maybe [String]
solve _ "" = Just []
solve passwords guess = search cs where
  cs = filter (\x -> isPrefixOf x guess) passwords
  -- search :: [String] -> [String]
  search []     = Nothing
  search (x:xs) = case solve passwords $ drop (length x) guess of
    Just ret -> Just (x:ret)
    Nothing  -> search xs
