module Main where

{-
- The number, 197, is called a circular prime because all rotations of the
- digits: 197, 971, and 719, are themselves prime.
-
- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37,
- 71, 73, 79, and 97.
-
- How many circular primes are there below one million?
-}

import Primes
import qualified Data.Set as S
import Data.Digits

-- | The list of primes below one million, computed by the Sieve of
-- Eratosthenes.
primes = primesBelow 1000000

-- | The set of primes below one million.
primeSet = S.fromList primes

-- | Rotates a list to the left.
--
-- > rotateL [1,2,3,4,5]
-- [2,3,4,5,1]
--
rotateL :: [a] -> [a]
rotateL [] = []
rotateL (x:xs) = xs ++ [x]

-- | The number of digits in a number, computed with logarithms.
digitCount = ceiling . logBase 10 . (+) 1 . fromIntegral

-- | Determine whether a number is a circular prime.
--
-- First a number is checked whether it has any even digits or contains the
-- digit `5` as a simple preliminary check to avoid otherwise expensive set
-- lookups, which are used as a more extensive primality check.
--
-- Special cases are added for 2 and 5 because they would fail the simple
-- check.
isCircularPrime :: S.Set Integer -> Integer -> Bool
isCircularPrime _ 2 = True
isCircularPrime _ 5 = True
isCircularPrime s n = (simpleCheck n) && (all (`S.member` s) $ rotationsOf n)
    where simpleCheck n = (not (5 `elem` ds)) && (not . any even $ ds)
            where ds = digits 10 n

rotationsOf n = take (digitCount n) -- as many numbers as there are digits
                $ map (unDigits 10) -- convert back to numbers
                $ iterate rotateL -- infinite list of leftward digit rotations
                $ digits 10 n -- get the digits of the number

-- | The set of all circular primes in the given set of primes.
circularPrimesIn ps = S.filter (isCircularPrime ps) ps

-- | The cardinality of the set of all circular primes.
circularPrimeCount = S.size . circularPrimesIn

main = print $ circularPrimeCount primeSet
