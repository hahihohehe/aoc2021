import System.Environment
import Data.List

main = do
  args <- getArgs
  contents <- readFile $ head args
  let l = lines contents
  let num = parseInputs $ head l
  putStrLn $ show $ minimum $ calcDist num

parseInputs :: String -> [Int]
parseInputs = map read . words . map comToSp where
   comToSp ',' = ' '
   comToSp x = x

calcDist :: [Int] -> [Int]
calcDist list = map (\x -> dist list x) [minimum list..maximum list]

dist :: [Int] -> Int -> Int
dist list x = (sum $ map (abs . (x-)) list) 
