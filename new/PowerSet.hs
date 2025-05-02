module PowerSet where

-- good read: https://jdhsmith.math.iastate.edu/class/BookOfProof.pdf

powerSet :: [Int] -> [[Int]]
powerSet []     = [[]]
powerSet (x:xs) = ps ++ map (x:) ps
 where
  ps = powerSet xs


main :: IO ()
main = do
 print (powerSet [])
 print (powerSet [1])
 print (powerSet [5])
 print (powerSet [1, 5])
 print (powerSet [1, 2, 3])