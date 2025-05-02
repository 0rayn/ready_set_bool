module ConjunctiveNormalForm where

import NegationNormalForm (Formula(..), formulaToRPN, turnIntoNNF, parseRPN)

----------------------------------------------------------------------------------------------
turnIntoCNF' :: Formula -> Formula
turnIntoCNF' (And a b) = And (turnIntoCNF' a) (turnIntoCNF' b)
turnIntoCNF' (Or a b)  = distribute (turnIntoCNF' a) (turnIntoCNF' b)
turnIntoCNF' other    = other
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
distribute :: Formula -> Formula -> Formula
distribute (And a b) c = And (distribute a c) (distribute b c)
distribute a (And b c) = And (distribute a b) (distribute a c)
distribute a b         = Or a b
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
flatten :: Formula -> Formula
flatten (And a b) = mkOp And (flattenAnd (flatten a) ++ flattenAnd (flatten b))
flatten (Or a b)  = mkOp Or (flattenOr (flatten a)  ++ flattenOr (flatten b))
flatten other     = other
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
flattenAnd :: Formula -> [Formula]
flattenAnd (And x y) = flattenAnd x ++ flattenAnd y
flattenAnd f         = [f]
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
flattenOr :: Formula -> [Formula]
flattenOr (Or x y) = flattenOr x ++ flattenOr y
flattenOr f        = [f]
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
mkOp :: (Formula -> Formula -> Formula) -> [Formula] -> Formula
mkOp _  []  = error "mkOp: empty"
mkOp _  [x] = x
mkOp op xs  = foldr1 op xs
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
turnIntoCNF :: Formula -> Formula
turnIntoCNF f = flatten (turnIntoCNF' f)
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
conjunctiveNormalForm :: String -> String
conjunctiveNormalForm str = formulaToRPN (turnIntoCNF (turnIntoNNF (parseRPN str)))
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
main :: String -> IO ()
main str = do
  print (conjunctiveNormalForm str)
----------------------------------------------------------------------------------------------