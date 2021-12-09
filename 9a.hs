import System.Environment
import Data.List

main = do
  args <- getArgs
  contents <- readFile $ head args
  let l = lines contents
  let m = parseInput l
  putStrLn $ show $ classify m
  putStrLn $ show $ sum $ map (sum . (map risk)) $ classify m

data Flag a = Small a | Big a deriving (Show, Eq, Ord)

value :: Flag a -> a
value (Small a) = a
value (Big a) = a

type Map = [[Flag Int]]

parseInput :: [String] -> Map
parseInput = map (map ((\x -> Small x) . read . (\x -> [x])))

classify :: Map -> Map
classify l = transpose $ map lineFilter $ transpose $ (map lineFilter l)

lineFilter :: [Flag Int] -> [Flag Int]
lineFilter list = fun list 100000 where
  fun (a:b:xs) last
    | value a < last && value a < value b = a : fun (b:xs) (value a)
    | otherwise = (Big (value a)) : fun (b:xs) (value a)
  fun (x:xs) last
    | value x < last = x : fun xs (value x)
    | otherwise = (Big (value x)) : fun xs (value x)
  fun [] _ = []

risk :: Flag Int -> Int
risk (Small a) = a + 1
risk (Big a) = 0
