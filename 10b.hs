import System.Environment
import Data.List

main = do
  args <- getArgs
  contents <- readFile $ head args
  let l = lines contents
  putStrLn $ show $ median $ filter (/=0) $ map lineScore l

lineScore :: String -> Int
lineScore s = fun s [] where
  fun (x:xs) stack
    | x == '(' = fun xs (')':stack)
    | x == '[' = fun xs (']':stack)
    | x == '<' = fun xs ('>':stack)
    | x == '{' = fun xs ('}':stack)
    | length stack > 0 && x == head stack = fun xs $ tail stack
    | x == ')' = 0
    | x == ']' = 0
    | x == '}' = 0
    | x == '>' = 0
  fun [] stack = calcScore stack

calcScore :: String -> Int
calcScore s = foldl fun 0 s where
  fun acc x
    | x == ')' = 1 + 5 * acc
    | x == ']' = 2 + 5 * acc
    | x == '}' = 3 + 5 * acc
    | x == '>' = 4 + 5 * acc

median :: [Int] -> Int
median l = (sort l) !! ((length l - 1) `div` 2)
