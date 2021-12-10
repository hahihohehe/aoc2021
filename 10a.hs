import System.Environment
import Data.List

main = do
  args <- getArgs
  contents <- readFile $ head args
  let l = lines contents
  putStrLn $ show $ sum $ map lineScore l

lineScore :: String -> Int
lineScore s = fun s [] where
  fun (x:xs) stack
    | x == '(' = fun xs (')':stack)
    | x == '[' = fun xs (']':stack)
    | x == '<' = fun xs ('>':stack)
    | x == '{' = fun xs ('}':stack)
    | length stack > 0 && x == head stack = fun xs $ tail stack
    | x == ')' = 3
    | x == ']' = 57
    | x == '}' = 1197
    | x == '>' = 25137
  fun [] _ = 0
