import System.Environment

main = do
  args <- getArgs
  contents <- readFile $ head args
  let gamma = (gammaRate . lines) contents
  let eps = (epsilonRate . lines) contents
  putStrLn $ show $ gamma
  putStrLn $ show $ eps
  putStrLn $ show $ (strToInt gamma) * (strToInt $ eps)
  let oxygenGeneratorRating = head $ filterRounds gammaRate $ lines contents
  let co2ScrubberRating = head $ filterRounds epsilonRate $ lines contents
  putStrLn oxygenGeneratorRating
  putStrLn co2ScrubberRating
  putStrLn $ show $ (strToInt oxygenGeneratorRating) * (strToInt co2ScrubberRating)

gammaRate :: [String] -> String
gammaRate =  biasToChar1 . foldr biasChange (repeat 0)

biasChange (x:xs) (a:as) = changeBias x a : biasChange xs as
biasChange [] _ = []

changeBias c acc
  | c == '1' = acc + 1
  | c == '0' = acc - 1

biasToChar1 (x:xs)
  | x >= 0 = '1' : biasToChar1 xs
  | x < 0 = '0' : biasToChar1 xs
biasToChar1 [] = []

biasToChar0 (x:xs)
  | x >= 0 = '0' : biasToChar0 xs
  | x < 0 = '1' : biasToChar0 xs
biasToChar0 [] = []

strToInt = foldl (\acc x -> 2 * acc + read [x] :: Int) 0

epsilonRate = biasToChar0 . foldr biasChange (repeat 0)

isChar :: Int -> Char -> String -> Bool
isChar index char str = str!!index == char

filterRounds :: ([String] -> String) -> [String] -> [String]
filterRounds bitmapGen = filterRound 0 where
  filterRound idx [x] = [x]
  filterRound idx lines = filterRound (idx+1) $ myFilter (isChar idx ((bitmapGen lines)!!idx)) lines


-- Filters until there is only one item left
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter cond list = myFilterAcc [] list where
  myFilterAcc [] [x] = [x]
  myFilterAcc acc (x:xs)
    | cond x = myFilterAcc (x:acc) xs
    | otherwise = myFilterAcc acc xs
  myFilterAcc acc [] = reverse acc
