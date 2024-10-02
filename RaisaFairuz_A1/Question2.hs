
import Prelude


bionomial :: Int -> Int -> Int 
bionomial n 0 = 1
bionomial 0 k = 0 
bionomial n k = bionomial (n-1) (k-1) * n `div` k