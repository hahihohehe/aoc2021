import System.Environment
import Data.List

main = do
  args <- getArgs
  contents <- readFile $ head args
  let l = lines contents
  let num = parseInputs $ head l
  let s = convertToStateList num
  let after80 = iterate day s !! 80
  putStrLn $ show $ sum after80

parseInputs :: String -> [Int]
parseInputs = map read . words . map comToSp where
   comToSp ',' = ' '
   comToSp x = x

convertToStateList :: [Int] -> [Int]
convertToStateList inputs = map (\i -> length $ filter (==i) inputs) [0,1,2,3,4,5,6,7,8]

day :: [Int] -> [Int]
day state = mapWithIndex create6 $ tail state ++ [head state] where
  create6 6 x = x + head state
  create6 i x = x

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex fun list = fst $ foldr (\x (acc,idx) -> (fun idx x:acc,idx-1)) ([],length list - 1) list
