import System.Environment
import Data.List

main = do
  args <- getArgs
  contents <- readFile $ head args

data Number = Pair Number Number Number | Regular Number Int | Root

parent :: Number -> Number
parent (Pair p _ _) = p
parent (Regular p _) = p

add :: Number -> Number -> Number
add a b = p where
  p = Pair (Root) pa pb
  pa = addParent p a
  pb = addParent p b

addParent :: Number -> Number -> Number
addParent p (Pair _ a b) = Pair p a b
addParent p (Regular _ i) = Regular i

parseNumber :: String -> Number -> Number
parseNumber ('[':xs) p = Pair p (parseNumber 

extractInner :: String -> Int -> String -> String
extractInner ('[':xs) i acc = extractInner xs (i+1) (acc ++ "[")
extractInner (']':xs) i acc
  | i == 1 = 
