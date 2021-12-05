import System.Environment
import Data.List

main = do
  args <- getArgs
  contents <- readFile $ head args
  let l = parseLines $ lines contents
  putStrLn $ show $ l
  putStrLn $ show $ findCrossings l
  putStrLn $ show $ length $ findCrossings l

type Pos = (Int, Int)
data Line = Horizontal Int Int Int | Vertical Int Int Int | NoLine deriving (Eq, Show)
type Board = [[Int]]

initBoard :: Board
initBoard = repeat (repeat -1)

putLineOnBoard :: Line -> Board -> board
putLineOnBoard l b
  | (Horizontal y x1 x2) = zipWith fun 

parseLines :: [String] -> [Line]
parseLines = filter (/= NoLine) . map parseLine where
  parseLine line = createLine (parsePos leftPos) (parsePos rightPos) where
    [leftPos, _, rightPos] = words line
    parsePos s = listToPos $ map read $ words $ map comToSp s where
      comToSp ',' = ' '
      comToSp x = x

listToPos (a:b:xs) = (a,b)

createLine :: Pos -> Pos -> Line
createLine (a,b) (a2,b2)
  | a == a2 && b < b2= Vertical a b b2
  | a == a2 = Vertical a b2 b
  | b == b2 && a < a2 = Horizontal b a a2
  | b == b2 = Horizontal b a2 a
  | otherwise = NoLine

separateLineTypes :: [Line] -> ([Line], [Line])
separateLineTypes line = sepAcc line ([], []) where
  sepAcc ((Horizontal x1 x2 x3):xs) (a, b) = sepAcc xs (a, (Horizontal x1 x2 x3):b)
  sepAcc ((Vertical x1 x2 x3):xs) (a, b) = sepAcc xs ((Vertical x1 x2 x3):a, b)
  sepAcc [] (a,b) = (a,b)

findCrossings xs = remDup $ sort $ foldl (++) [] [crossLines x y | x <- xs, y <- xs, x /= y]

remDup :: [Pos] -> [Pos]
remDup (l:ls) = fun [l] ls where
  fun (a:as) (x:xs)
    | a == x = a:xs
    | otherwise = x:a:xs
  fun acc [] = acc

crossLines :: Line -> Line -> [Pos]
crossLines (Vertical b a1 a2) (Horizontal a b1 b2) = crossLines (Horizontal a b1 b2) (Vertical b a1 a2)
crossLines (Horizontal a b1 b2) (Vertical b a1 a2)
  | a1 <= a && a <= a2 && b1 <= b && b <= b2 = [(b, a)]
  | otherwise = []
crossLines (Horizontal a1 b11 b12) (Horizontal a2 b21 b22)
  | a1 == a2 && (b11 >= b21 && b11 <= b22) = [(b, a1) | b <- [b21..b11]]
  | a1 == a2 && (b12 >= b21 && b12 <= b22) = [(b, a1) | b <- [b12..b22]]
  | otherwise = []
crossLines (Vertical a1 b11 b12) (Vertical a2 b21 b22)
  | a1 == a2 && (b11 >= b21 && b11 <= b22) = [(a1,b) | b <- [b21..b11]]
  | a1 == a2 && (b12 >= b21 && b12 <= b22) = [(a1,b) | b <- [b12..b22]]
  | otherwise = []
  
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
