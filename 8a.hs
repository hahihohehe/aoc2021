import System.Environment

main = do
  args <- getArgs
  contents <- readFile $ head args
  let l = lines contents
  let codes = map (drop 11 . words) l
  let res = sum $ map (length . filter (\x -> length x `elem` [2,3,4,7])) codes
  putStrLn $ show $ res
