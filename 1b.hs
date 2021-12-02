import System.Environment

main = do
  args <- getArgs
  contents <- readFile $ head args
  putStrLn $ show $ (countIncreases . slidingWindow . map read . words) contents

countIncreases :: [Int] -> Int
countIncreases (x:xs) = snd $ foldl (\(last, acc) e -> if e > last then (e, acc+1) else (e, acc)) (x, 0) xs

slidingWindow :: [Int] -> [Int]
slidingWindow (a:b:xs) = reverse $ tail $ tail $ foldl (\(x1:x2:xx) e -> x2:e:(x1+x2+e):xx) [a,b] xs
