module GrayCode where
import Data.Bits (xor, shiftR)

grayCode :: Int -> Int
grayCode n = n `xor` (n `shiftR` 1)
