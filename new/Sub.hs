module Sub where
import Adder (adder)
import Data.Bits (complement)
import Data.Word (Word32)

sub :: Word32 -> Word32 -> Word32

sub nbr1 nbr2 = adder nbr1 ((complement nbr2) + 1)