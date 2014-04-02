import Data.List

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

firstDivisors :: Integer -> [Integer]
firstDivisors n
    | n == 0 = []
    | otherwise = divisors' n 1
        where divisors' :: Integer -> Integer -> [Integer]
              divisors' n d
                  | d > (floor $ sqrt $ fromInteger n) = []
                  | n `mod` d == 0 = d:(divisors' n (d + 1))
                  | otherwise = divisors' n (d + 1)

sq n = n * n
isSquareNumber n = n == sq (floor $ sqrt $ fromInteger n)

properDivisors :: Integer -> [Integer]
properDivisors n = let ds = firstDivisors n
                in (if isSquareNumber n then init ds else ds) ++ init (reverse $ map (\n' -> n `div` n') ds)

divisorSum :: Integer -> Integer
divisorSum n = sum . properDivisors $ n

isAbundant :: Integer -> Bool
isAbundant n = divisorSum n > n

isDeficient :: Integer -> Bool
isDeficient n = divisorSum n < n

isPerfect :: Integer -> Bool
isPerfect n = divisorSum n == n

upperBound = 28123

range = [1..28123]

abundants = filter isAbundant range

abundantSums = concat $ map (\x -> map (\y -> if x + y < upperBound then x + y else 0) abundants) abundants

isNotSumOfAbundants n = null $ filter (isAbundant) $ map (\x -> n - x) $ takeWhile (<=n `div` 2) abundants 
--isSumOfAbundants n = length (take 1 $ filter(\x -> x > 0 && isAbundant x) $  map (\y -> n - y) (takeWhile (<=n `div` 2) abundants)) >= 1

--main = print $ sum $ filter isNotSumOfAbundants range
