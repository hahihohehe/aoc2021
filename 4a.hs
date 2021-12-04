import System.Environment

main = do
  args <- getArgs
  contents <- readFile $ head args
  let l = lines contents
  let num = parseInputs $ head l
  let boards = parseBoards $ tail l
  putStrLn $ show num
  putStrLn $ show boards
  

type Board = [[Int]]

parseBoards :: [String] -> [Board]
parseBoards ("":xs) = parseBoards xs
parseBoards (a:b:c:d:e:xs) = map (map read . words) [a,b,c,d,e] : parseBoards xs
parseBoards [] = []

parseInputs :: String -> [Int]
parseInputs = map read . words . map comToSp where
   comToSp ',' = ' '
   comToSp x = x
