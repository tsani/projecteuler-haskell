import Data.Ratio

-- some useful code for dealing with prime numbers

primeFactors :: Integer -> [Integer]
primeFactors n = filter (\p -> n `mod` p == 0) $ primesBelow n

-- Get all the prime numbers below a given limit by using the sieve of Erastosthenes
primesBelow :: Integer -> [Integer]
primesBelow lim 
    | abs lim == 1 = [1]
    | abs lim == 2 = [2]
    | otherwise    = multipleFilter' [2..lim] []
    where multipleFilter' :: [Integer] -> [Integer] -> [Integer]
          multipleFilter' (p:ns) ps 
              | p > (ceiling . sqrt . fromInteger) lim = reverse (p:ps) ++ ns
              | otherwise = multipleFilter' (filter (\x -> x `mod` p /= 0) ns) (p:ps)

maxPair :: Ord b => [(a, b)] -> (a, b)
maxPair [] = error "Cannot find maximum in empty list."
maxPair ps = maxPair' ps (ps !! 0)
    where maxPair' [] l = l
          maxPair' ((t', s'):ps) (curMaxTree, curMaxSum) = if s' > curMaxSum 
                                                           then maxPair' ps (t', s')
                                                           else maxPair' ps (curMaxTree, curMaxSum)

dupe f x = (x, f x)

isSeqInfinite d = not $ any (`elem` [2,5]) (primeFactors d)

mk9 x = take x $ repeat 9

--dropWhile (\x -> floor (x / 7) /= x / 7)

mkSeq :: Integer -> Integer
mkSeq y  = flip div y $ flip (!!) 0 $ dropWhile (\x -> x `mod` y /= 0 ) $ map (\x -> (read $ take x $ repeat '9') :: Integer) [1..]

answer = fst $ maxPair $ map (dupe $ length . show . mkSeq) (tail $ tail $ tail $ primesBelow 1000)

main = print answer
