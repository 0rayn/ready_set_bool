module TruthTable where

import EvalFormula (evalFormula)
import Data.Char (isUpper)  -- to check if a character is a capital letter
import Data.List (nub)      -- to remove duplicates
import Data.Bits (shiftL, testBit)


---------------------------------------------------------------------
twoPowerN :: Int -> Int
twoPowerN n = 1 `shiftL` n
---------------------------------------------------------------------

------------------------------------------------------------
showBit :: Bool -> String
showBit True = "1"
showBit false = "0"
------------------------------------------------------------

---------------------------------------------------------------------
getMaxIterations :: [(Int, Char, Bool)] -> Int
getMaxIterations vars = twoPowerN (length vars)
---------------------------------------------------------------------

---------------------------------------------------------------------
-- new concept: list comprehension
getVariables :: String -> [(Int, Char, Bool)]
getVariables formula =
 let vars = [(c, False) | (c) <- (nub formula), isUpper c]
 in [(pos, c, val) | (pos, (c, val)) <- zip [0..] vars]
---------------------------------------------------------------------

---------------------------------------------------------------------
invertVariablesState :: [(Int, Char, Bool)] -> Int -> [(Int, Char, Bool)]
invertVariablesState vars currentIter = [(pos, c, testBit currentIter pos) | (pos, c, _) <- vars]
---------------------------------------------------------------------

---------------------------------------------------------------------
replaceWithValues :: String -> [(Int, Char, Bool)] -> String
replaceWithValues formula vars =
    concat [if isUpper c then showBit (lookupBool c) else [c] | c <- formula]
  where
    lookupBool c = case lookup c [(ch, val) | (_, ch, val) <- vars] of
                     Just v  -> v
                     Nothing -> error "this doesn't make sense ?!"
---------------------------------------------------------------------

---------------------------------------------------------------------
printHeader :: [(Int, Char, Bool)] -> IO ()
printHeader vars = do
  -- Print the header row with variable names
  mapM_ (\(_, c, _) -> putStr "| " >> putStr [c] >> putStr " ") vars
  putStr "| = |\n"

  -- Print the separator row
  mapM_ (\_ -> putStr "|---") vars
  putStr "|---|\n"
---------------------------------------------------------------------

---------------------------------------------------------------------
printResultRow :: [(Int, Char, Bool)] -> Bool -> IO ()
printResultRow vars res = do
 mapM_ (\(_, _, v) -> putStr "| " >> putStr (showBit v) >> putStr " ") vars
 putStr "| " >> putStr (showBit res) >> putStr " |\n"
---------------------------------------------------------------------

---------------------------------------------------------------------
evaluateAndPrint :: String -> [(Int, Char, Bool)] -> Int -> Int -> IO ()
evaluateAndPrint formula vars currentIter maxIterations
 | currentIter >= maxIterations = return ()
 | otherwise = do
  let newVars = invertVariablesState vars currentIter
  let newFormula = replaceWithValues formula newVars
  let result = evalFormula newFormula
  printResultRow newVars result
  evaluateAndPrint formula newVars (currentIter + 1) maxIterations
---------------------------------------------------------------------

---------------------------------------------------------------------
truthTable :: String -> IO ()
truthTable formula  = do
 let vars = getVariables formula
 let maxIterations = getMaxIterations vars
 printHeader vars
 evaluateAndPrint formula vars 0 maxIterations
--  let vars = getVariables(formula)
--  let currentIter = 4
--  let result = invertVariablesState vars currentIter
--  let newFormula = replaceWithValues formula result
--  -- Print just the True/False values
----  print [state | (_, _, state) <- result]
--  print vars
--  print newFormula
-------------------------------------------------------------------