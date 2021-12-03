import System.Environment

main = do
  args <- getArgs
  contents <- readFile $ head args
  let gamma = (gammaRate . lines) contents
  putStrLn $ show $ gamma
  putStrLn $ show $ epsilonRate gamma
  putStrLn $ show $ (strToInt gamma) * (strToInt $ epsilonRate gamma)

gammaRate :: [String] -> String
gammaRate =  biasToChar . foldr biasChange (repeat 0)

biasChange (x:xs) (a:as) = changeBias x a : biasChange xs as
biasChange [] _ = []

changeBias c acc
  | c == '1' = acc + 1
  | c == '0' = acc - 1

biasToChar (x:xs)
  | x > 0 = '1' : biasToChar xs
  | x < 0 = '0' : biasToChar xs
biasToChar [] = []

strToInt = foldl (\acc x -> 2 * acc + read [x] :: Int) 0

epsilonRate (x:xs)
  | x == '1' = '0' : epsilonRate xs
  | x == '0' = '1' : epsilonRate xs
epsilonRate [] = []
