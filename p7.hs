dumbFactors :: Int -> [Int]
dumbFactors x = dumbFactors' x 2
    where dumbFactors' :: Int -> Int -> [Int]
          dumbFactors' x' divisor
              | divisor <= x' = 
                  if x' `mod` divisor == 0  
                  then divisor:(dumbFactors' (x' `div` divisor) (divisor + 1))
                  else dumbFactors' x' (divisor + 1)
              | otherwise = []

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x = (length $ n) == 1 && n !! 0 == x
    where n = dumbFactors x

primes :: [Int]
primes = primes' 2
    where primes' :: Int -> [Int]
          primes' x
              | isPrime x = x:(primes' $ x + 1)
              | otherwise = primes' $ x + 1
        
main = do
    print $ primes !! 10000
