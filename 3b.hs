import System.Environment

main = do
  args <- getArgs
  contents <- readFile $ head args
  let gamma = (gammaRate . lines) contents
  putStrLn $ show $ gamma
  putStrLn $ show $ epsilonRate gamma
  putStrLn $ show $ (strToInt gamma) * (strToInt $ epsilonRate gamma)
  let oxygenGeneratorRating = head $ applyFilters (oxFilters $ lines contents) $ lines contents
  let co2ScrubberRating = head $ applyFilters (co2Filters $ lines contents) $ lines contents
  putStrLn oxygenGeneratorRating
  putStrLn co2ScrubberRating
  putStrLn $ show $ (strToInt oxygenGeneratorRating) * (strToInt co2ScrubberRating)

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

isChar :: Int -> Char -> String -> Bool
isChar index char str = str!!index == char

oxFilters :: [String] -> [String -> Bool]
oxFilters lines = map (\index -> isChar index ((gammaRate lines)!!index)) [0..(length $ head lines)-1]

co2Filters :: [String] -> [String -> Bool]
co2Filters lines = map (\index -> isChar index ((epsilonRate $ gammaRate lines)!!index)) [0..(length $ head lines)-1]

applyFilters :: [a -> Bool] -> [a] -> [a]
applyFilters (f:fs) list = applyFilters fs $ myFilter f list
applyFilters _ [x] = [x]

-- Filters until there is only one item left
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter cond list = myFilterAcc [] list where
  myFilterAcc [] [x] = [x]
  myFilterAcc acc (x:xs)
    | cond x = myFilterAcc (x:acc) xs
    | otherwise = myFilterAcc acc xs
  myFilterAcc acc [] = reverse acc
