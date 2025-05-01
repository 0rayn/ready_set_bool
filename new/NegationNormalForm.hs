module NegationNormaForm where

-- declaring a new var type with multiple constructors
data Formula = Var Char
 | Not Formula
 | And Formula Formula
 | Or Formula Formula
 deriving Show -- Automatically generate code for printing and debuging

parseRPN :: String -> formula

parseRPN str = foldl step [] str
 where step [] str
  | step (x:y:rest) '&' = And x y : rest
  | step (x:y:rest) '|' = Or x y : rest
  | step (x:rest) '!' = Not x : rest
  | step rest c = Var c : rest



main :: String -> IO ()

main str = do
	print (parseRPN str)
