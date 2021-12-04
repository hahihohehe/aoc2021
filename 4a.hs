import System.Environment

main = do
  args <- getArgs
  contents <- readFile $ head args
  let l = lines contents
  let num = parseInputs $ head l
  let boards = parseBoards $ tail l
  putStrLn $ show num
  putStrLn $ show boards
  putStrLn $ show $ performAllRounds num boards
  putStrLn $ show $ findFirstWin num boards
  putStrLn $ show $ (score $ snd $ findFirstWin num boards) * (fst $ findFirstWin num boards)
  

type Board = [[Int]]

parseBoards :: [String] -> [Board]
parseBoards ("":xs) = parseBoards xs
parseBoards (a:b:c:d:e:xs) = map (map read . words) [a,b,c,d,e] : parseBoards xs
parseBoards [] = []

parseInputs :: String -> [Int]
parseInputs = map read . words . map comToSp where
   comToSp ',' = ' '
   comToSp x = x

markNumber :: Int -> Board -> Board
markNumber i b = map (map mark) b where
  mark x
    | x == i = -1
    | otherwise = x

performRound :: Int -> [Board] -> [Board]
performRound i = map (markNumber i)

performAllRounds :: [Int] -> [Board] -> [Board]
performAllRounds (x:xs) boards = performAllRounds xs $ performRound x boards
performAllRounds [] boards = boards

checkWin :: Board -> Bool
checkWin b = checkRows b || checkColumns b where
  checkRows (x:xs)
    | sum x == -5 = True
    | otherwise = checkWin xs
  checkRows [] = False
  checkColumns b = any (\x -> x) (foldl (\acc x -> (sum (map (!!x) b) == -5) : acc) [] [0,1,2,3,4])

findWins :: [Board] -> [Board]
findWins = filter checkWin

findFirstWin :: [Int] -> [Board] -> (Int, Board)
findFirstWin (x:xs) boards
  | (length $ findWins $ performRound x boards) > 0 = (x, head $ findWins $ performRound x boards)
  | otherwise = findFirstWin xs $ performRound x boards

score :: Board -> Int
score b = sum $ map sum $ map (map rM1) b where
  rM1 x
    | x == -1 = 0
    | otherwise = x
