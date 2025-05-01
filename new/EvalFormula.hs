module EvalFormula where

-- foldl/foldr https://www.youtube.com/watch?v=0qvi_sTJbEw

type Stack = [Bool]

evalFormula :: String -> Bool
evalFormula' :: String -> Stack -> Bool
pushBit :: Stack -> Char -> Stack
evalOp :: Stack -> Char -> Stack

------------------------------------------------------------------------------
pushBit stack '1' = True : stack
pushBit stack '0' = False : stack
pushBit _ c = error ("Invalid bit: " ++ [c])
------------------------------------------------------------------------------

------------------------------------------------------------------------------
evalOp (b:a:rest) '&' = (a && b) : rest
evalOp (b:a:rest) '|' = (a || b) : rest
evalOp (b:a:rest) '>' = (not a || b) : rest  -- implication
evalOp (b:a:rest) '=' = (a == b) : rest      -- equality
evalOp _ c = error ("Unknown op or not enough operands: " ++ [c])
------------------------------------------------------------------------------

------------------------------------------------------------------------------
    -- if we finished the string
evalFormula' [] [result] = result
    -- if the stack has something else and not only the result
evalFormula' [] _        = error "Invalid formula: stack not empty at end"
    -- normal case there is something in the string
evalFormula' (c:cs) stack -- c is the first element of the string
  | c == '1' || c == '0' = evalFormula' cs (pushBit stack c)         -- push bits
  | c `elem` "&|>="      = evalFormula' cs (evalOp stack c)          -- apply operators
  | otherwise            = error ("Invalid character: " ++ [c])     -- invalid character
------------------------------------------------------------------------------

------------------------------------------------------------------------------
evalFormula proposition = evalFormula' proposition []
------------------------------------------------------------------------------