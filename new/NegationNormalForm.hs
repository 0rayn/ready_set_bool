module NegationNormalForm where

data Formula = Var Char
             | Not Formula
             | And Formula Formula
             | Or Formula Formula
             deriving Show -- Automatically generate code for printing and debugging

-----------------------------------------------------------------------------
parseRPN :: String -> Formula
parseRPN str =
  case foldl step [] str of
    [result] -> result
    _ -> error "Invalid formula"
  where
    step :: [Formula] -> Char -> [Formula]
    step (b:a:rest) '&' = And a b : rest
    step (b:a:rest) '|' = Or a b : rest
    step (b:a:rest) '^' = Or (And (Not a) b) (And (Not b) a) : rest
    step (b:a:rest) '>' = Or (Not a) b : rest
    step (b:a:rest) '=' = Or (And a b) (And (Not a) (Not b)) : rest
    step (a:rest) '!' = Not a : rest
    step rest c = Var c : rest
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
turnIntoNNF :: Formula -> Formula
turnIntoNNF (Var c) = Var c
turnIntoNNF (Not (Var c)) = Not (Var c)
turnIntoNNF (Not (Not f)) = turnIntoNNF f
turnIntoNNF (Not (And f1 f2)) = Or (turnIntoNNF (Not f1)) (turnIntoNNF (Not f2))
turnIntoNNF (Not (Or f1 f2)) = And (turnIntoNNF (Not f1)) (turnIntoNNF (Not f2))
turnIntoNNF (And f1 f2) = And (turnIntoNNF f1) (turnIntoNNF f2)
turnIntoNNF (Or f1 f2) = Or (turnIntoNNF f1) (turnIntoNNF f2)
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
formulaToRPN :: Formula -> String
formulaToRPN (Var c) = [c]
formulaToRPN (Not f) = formulaToRPN f ++ "!"
formulaToRPN (And f1 f2) = formulaToRPN f1 ++ formulaToRPN f2 ++ "&"
formulaToRPN (Or f1 f2) = formulaToRPN f1 ++ formulaToRPN f2 ++ "|"
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
processFormula :: String -> String
processFormula = formulaToRPN . turnIntoNNF . parseRPN
-----------------------------------------------------------------------------

main :: String -> IO ()
main str = do
    let parsedFormula = parseRPN str
    let nffFormula = turnIntoNNF parsedFormula
    putStrLn "Parsed formula:"
    print parsedFormula
    putStrLn "In Negation Normal Form:"
    print (nffFormula)
    putStrLn "In RPN:"
    print (formulaToRPN nffFormula)
    -- print (processFormula str)