module EvalFormula where
import Data.Bits (xor)
-- foldl/foldr https://www.youtube.com/watch?v=0qvi_sTJbEw

type Stack = [Bool]


------------------------------------------------------------------------------
pushBit :: Stack -> Char -> Stack
pushBit stack '1' = True : stack
pushBit stack '0' = False : stack
pushBit _ c = error ("Invalid bit: " ++ [c])
------------------------------------------------------------------------------

------------------------------------------------------------------------------
evalOp :: Stack -> Char -> Stack
evalOp   (a:rest) '!' = (not a): rest
evalOp (b:a:rest) '&' = (a && b) : rest
evalOp (b:a:rest) '|' = (a || b) : rest
evalOp (b:a:rest) '^' = (a `xor` b) : rest
evalOp (b:a:rest) '>' = (not a || b) : rest
evalOp (b:a:rest) '=' = (a == b) : rest
evalOp _ c = error ("Unknown op or not enough operands: " ++ [c])
------------------------------------------------------------------------------

------------------------------------------------------------------------------
evalFormula' :: String -> Stack -> Bool
evalFormula' [] [result] = result
evalFormula' [] _        = error "Invalid formula: stack not empty at end"
evalFormula' (c:cs) stack
  | c == '1' || c == '0' = evalFormula' cs (pushBit stack c)        -- push bits
  | c `elem` "!&|^>="      = evalFormula' cs (evalOp stack c)         -- apply operators
  | otherwise            = error ("Invalid character: " ++ [c])     -- invalid character
------------------------------------------------------------------------------

------------------------------------------------------------------------------
evalFormula :: String -> Bool
evalFormula proposition = evalFormula' proposition []
------------------------------------------------------------------------------
--main :: IO ()
--main = do
--  let stack = [1, 2, 3, 4]
--  let a:b:stackRest = stack  -- pattern matching on the stack
--  let newStack = 5:stack
--  print a
--  print b
--  print stackRest
--  print newStack
