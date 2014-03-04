bignum :: Int
bignum = 600851475143

dumbFactors :: Int -> [Int]
dumbFactors x = dumbFactors' x 2
    where dumbFactors' :: Int -> Int -> [Int]
          dumbFactors' x' divisor
              | divisor <= x' = 
                  if x' `mod` divisor == 0  
                  then divisor:(dumbFactors' (x' `div` divisor) (divisor + 1))
                  else dumbFactors' x' (divisor + 1)
              | otherwise = []
