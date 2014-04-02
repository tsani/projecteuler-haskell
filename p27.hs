-- some useful code for dealing with prime numbers
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
properDivisors n'' = let n = abs n'' in 
                         case firstDivisors n of
                            [] -> []
                            ds -> (if isSquareNumber n then init ds else ds) ++ init (reverse $ map (\n' -> n `div` n') ds)

maxPair :: Ord b => [(a, b)] -> (a, b)
maxPair [] = error "Cannot find maximum in empty list."
maxPair ps = maxPair' ps (ps !! 0)
    where maxPair' [] l = l
          maxPair' ((t', s'):ps) (curMaxTree, curMaxSum) = if s' > curMaxSum 
                                                           then maxPair' ps (t', s')
                                                           else maxPair' ps (curMaxTree, curMaxSum)

isPrime n = length (properDivisors n) == 1

incredible a b n = n ^ 2 + a * n + b

eulerIncredible = incredible 1 41

incredible2 = incredible (-79) 1601

conseqPrimes (a,b) = takeWhile isPrime (map (incredible a b) [0..])

l = [(a, b) | a <- [-999..999], b <- [-999..999]]

enumerate ls = zip [0..] ls
enumerateNat ls = zip [1..] ls

answer = let ((a,b), _) = maxPair $ map (\xt -> (xt, length $ conseqPrimes xt)) l in (a * b)

main = print $ answer
