import System.Environment
import Data.List
import Data.List.Index

main = do
  args <- getArgs
  contents <- readFile $ head args
  let l = lines contents
  let m = parseInput l
  putStrLn $ show $ product $ take 3 $ reverse $ sort $ extractBasins $ findBasins m

type Map = [[Int]]

parseInput :: [String] -> Map
parseInput = map (map (read . (\x -> [x])))

overlap :: (Eq a) => [a] -> [a] -> Bool
overlap l = not . null . intersect l

findBasins :: Map -> [(Int,[Int])]
findBasins m = foldl includeOld [] lines where
  lines = map convertLine m

extractBasins :: [(Int,[Int])] -> [Int]
extractBasins = map fst

includeOld :: [(Int,[Int])] -> [(Int,[Int])] -> [(Int,[Int])]
includeOld (x:xs) new = includeOld xs $ transformWithOld x new
includeOld [] new = new

findOverlapping :: [Int] -> [(Int,[Int])] -> [(Int,[Int])]
findOverlapping la lb = filter (\(_,l) -> overlap l la) lb

transformWithOld :: (Int,[Int]) -> [(Int,[Int])] -> [(Int,[Int])]
transformWithOld (a,al) list
  | null ol = (a,[]) : list
  | otherwise = mergeEntry a ol : remaining where
    ol = findOverlapping al list
    remaining = filter (not . (\x -> x `elem` ol)) list

mergeEntry :: Int -> [(Int,[Int])] -> (Int,[Int])
mergeEntry a list = (a + (sum $ map fst list), foldl (++) [] $ map snd list)

convertLine :: [Int] -> [(Int,[Int])]
convertLine = ifoldl fun [(0,[])] where
  fun ((a,alist):as) i x
    | x == 9 = (0,[]) : (a,alist) : as
    | otherwise = (a+1, i:alist) : as
