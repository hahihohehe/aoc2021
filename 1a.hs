import System.Environment

main = do
  args <- getArgs
  contents <- readFile $ head args
  putStrLn $ show $ (countIncreases . map read . words) contents

countIncreases :: [Int] -> Int
countIncreases (x:xs) = countIncreasesAcc xs 0 x where
  countIncreasesAcc [] acc _ = acc
  countIncreasesAcc (x:xs) acc last
    | x > last = countIncreasesAcc xs (acc+1) x
    | otherwise = countIncreasesAcc xs acc x
