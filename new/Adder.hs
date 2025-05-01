module Adder where
import Data.Word (Word32)
import Data.Bits (shiftL, xor, testBit, (.&.), (.|.))

halfAdder :: Bool -> Bool -> (Bool, Bool)
fullAdder :: Bool -> Bool -> Bool -> (Bool, Bool)
fullAdderPos :: Word32 -> Word32 -> Bool -> Int -> (Bool, Bool)
adder :: Word32 -> Word32 -> Word32

----------------------------------------------------------------
halfAdder bit1 bit2 = (sum, carry)
 where
    sum = bit1 `xor` bit2
    carry = bit1 .&. bit2
----------------------------------------------------------------

----------------------------------------------------------------
fullAdder bit0 bit1 prevCarry = (sum, carry)
 where
    (halfAdderSum, halfAdderCarry) = halfAdder bit0 bit1
    sum = halfAdderSum `xor` prevCarry
    carry = halfAdderCarry .|. (halfAdderSum .&. prevCarry)
----------------------------------------------------------------

----------------------------------------------------------------
fullAdderPos nbr1 nbr2 prevCarry pos = 
 let
  bit1 = testBit nbr1 pos
  bit2 = testBit nbr2 pos
 in
  fullAdder bit1 bit2 prevCarry
----------------------------------------------------------------

----------------------------------------------------------------
adder nbr1 nbr2 = aux 0 False 0
 where 
  aux :: Int -> Bool -> Word32 -> Word32
  aux pos prevCarry res
   | pos == 32 = res
   | otherwise = aux (pos + 1) newCarry newRes
    where
     (sumBit, newCarry) = fullAdderPos nbr1 nbr2 prevCarry pos
     sumWord32 = if sumBit then 1 `shiftL` pos else 0
     newRes = res .|. sumWord32
----------------------------------------------------------------

