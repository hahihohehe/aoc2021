import System.Environment

main = do
  args <- getArgs
  contents <- readFile $ head args
  putStrLn $ show $ (endPos . map (convertCommand . words) . lines) contents

convertCommand [c, i] = (c, read i :: Int)

endPos :: Num a => [(String, a)] -> (a, a, a)
endPos = foldl performStep (0,0,0)

performStep :: Num a => (a, a, a) -> (String, a) -> (a, a, a)
performStep (pos, depth, aim) (c, i)
  | c == "forward" = (pos+i, depth + (aim * i), aim)
  | c == "down" = (pos, depth, aim + i)
  | c == "up" = (pos, depth, aim - i)
