module SAT where

import EvalFormula (evalFormula)
import TruthTable (getMaxIterations, getVariables, replaceWithValues, invertVariablesState)

-- Future : https://www.youtube.com/watch?v=xFpndTg7ZqA dllp algo

-----------------------------------------------------------------------
sat' :: String -> [(Int, Char, Bool)] -> Int -> Int -> Bool -> Bool
sat' f vars currentIter maxIterations answer
 | currentIter >= maxIterations = answer
 | answer == True               = answer
 | otherwise                    = sat' f newVars (currentIter + 1) maxIterations newAnswer
  where
   newAnswer = evalFormula (replaceWithValues f vars)
   newVars = invertVariablesState vars (currentIter + 1)
-----------------------------------------------------------------------

-----------------------------------------------------------------------
sat :: String -> Bool
sat f = sat' f vars 0 maxIterations False
 where
  vars = getVariables f
  maxIterations = getMaxIterations vars
-----------------------------------------------------------------------

main :: String -> IO ()
main s = do
 print (sat s)