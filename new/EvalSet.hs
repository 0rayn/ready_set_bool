module EvalSet where

import Data.List (nub)      -- to remove duplicates
import ConjunctiveNormalForm (conjunctiveNormalForm)

type Stack = [[Int]]


--------------------------------------------------------------------------
evalSet' :: String -> [[Int]] -> Stack -> [Int]
evalSet' [] _ [result] = result
evalSet' [] _ stack = error "Invalid formula: stack not empty at end."
evalSet' (c:cs) sets stack
 | c `elem` ['A'..'Z'] = evalSet' cs sets (pushSet c sets stack)
 | c `elem` "!&|" = evalSet' cs sets (evalOp stack c)
 | otherwise = error ("Invalid character: " ++ [c])     -- invalid character
--------------------------------------------------------------------------

--------------------------------------------------------------------------
pushSet :: Char -> [[Int]] -> [[Int]] -> Stack
pushSet c sets stack = (sets !! (indexOf c)) : stack
  where
    indexOf :: Char -> Int
    indexOf ch = fromEnum ch - fromEnum 'A'
--------------------------------------------------------------------------

--------------------------------------------------------------------------
evalOp :: Stack -> Char -> Stack
evalOp (set1:set2:rest) '&' = (set1 `intersect` set2) : rest
evalOp (set1:set2:rest) '|' = (set1 `union` set2) : rest
evalOp (set:rest) '!' = (complement set (set:rest)) : rest
--------------------------------------------------------------------------

--------------------------------------------------------------------------
intersect :: [Int] -> [Int] -> [Int]
intersect set1 set2 = [x | x <- set1, x `elem` set2]
--------------------------------------------------------------------------

--------------------------------------------------------------------------
union :: [Int] -> [Int] -> [Int]
union set1 set2 = nub (set1 ++ [x | x <- set2, x `notElem` set1])
--------------------------------------------------------------------------

--------------------------------------------------------------------------
complement :: [Int] -> [[Int]] -> [Int]
complement set sets = aux set (nub (concat sets))
 where
  aux set universalSet = [x | x <- universalSet,  x `notElem` set]
--------------------------------------------------------------------------

--------------------------------------------------------------------------
evalSet :: String -> [[Int]] -> [Int]
evalSet proposition sets = evalSet' (conjunctiveNormalForm proposition) sets []
--------------------------------------------------------------------------

main :: IO ()
main = do
 let sets = [[0, 1, 2], [0, 3, 4]]
 let prop1 = "AB&";
 let prop2 = "AB|";
 let prop3 = "A!B|";
 let prop4 = "A!";
 print (evalSet prop1 sets)
 print (evalSet prop2 sets)
 print (evalSet prop3 sets)
 print (evalSet prop4 sets)