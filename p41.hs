import Primes
import Data.Digits

import Data.List ( sort, permutations )

isUnique :: Eq a => [a] -> Bool
isUnique [] = True
isUnique [x] = True
isUnique (x:y:xs) = if x == y then False else isUnique (y:xs)

isPandigital n = if head ds == 0 
                     then False
                     else isUnique ds
    where ds = sort $ digits 10 n

isPrime :: Integer -> Bool
isPrime n = not $ any ((==0) . mod n) numbersToCheck
    where numbersToCheck = [2..(floor $ sqrt $ fromIntegral n)]

pandigitalPrimes n = filter isPrime $ map (unDigits 10) $ permutations [1..n]

answer = last $ sort $ concatMap pandigitalPrimes [2..9]
