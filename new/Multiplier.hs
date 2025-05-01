module Multiplier where 
import Adder (adder)
import Data.Word (Word32)

multiplier :: Word32 -> Word32 -> Word32

multiplier nbr1 nbr2 = aux nbr1 nbr2 0
 where
  aux nbr1 nbr2 res
   | nbr2 == 0 = res
   | otherwise = aux nbr1 (nbr2 - 1) (adder res nbr1)