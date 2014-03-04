
primeFactors :: Integer -> [Integer]
primeFactors n = filter (\p -> n `mod` p == 0) $ primesBelow n

-- Get all the prime numbers below a given limit by using the sieve of Erastosthenes
primesBelow :: Integer -> [Integer]
primesBelow lim = multipleFilter' [2..lim] []
    where multipleFilter' :: [Integer] -> [Integer] -> [Integer]
          --multipleFilter' [] ps = ps
          multipleFilter' (p:ns) ps 
              | p >= (ceiling . sqrt . fromInteger) lim = (p:ps) ++ ns
              | otherwise = multipleFilter' (filter (\x -> x `mod` p /= 0) ns) (p:ps)
