module Multiplier where 
import Adder (adder)
import Data.Word (Word32)
import Data.Bits (testBit, shiftL)

multiplier :: Word32 -> Word32 -> Word32
multiplier' :: Word32 -> Word32 -> Int -> Word32 -> Word32
multiplyBit :: Word32 -> Bool -> Int -> Word32
getBit :: Word32 -> Int -> Bool

------------------------------------------------------
getBit number pos = testBit number pos
------------------------------------------------------

------------------------------------------------------ 
multiplyBit nbr1 bit pos = if bit then nbr1 `shiftL` pos else 0
------------------------------------------------------ 

------------------------------------------------------ 
multiplier' nbr1 nbr2 pos res
  | pos >= 32 = res
  | otherwise = multiplier' nbr1 nbr2 (pos + 1) newRes
   where
    newRes = (res + multiplyBit nbr1 (getBit nbr2 pos) pos)

------------------------------------------------------ 

------------------------------------------------------ 
multiplier nbr1 nbr2 = multiplier' nbr1 nbr2 0 0
------------------------------------------------------ 
