import Primes
import Data.Digits

import Data.List ( permutations, sort )

primes = sort $ primesBelow 25

pandigitals = permutations [0..9]

substrings (x:y:[]) = []
substrings (x:y:z:xs) = [x,y,z] : substrings (y:z:xs)

predicate = all id . 
            zipWith ($) subPredicates .
            map (unDigits 10) .
            subs
    where subPredicates = map (\n -> (/// n)) $ take 7 primes
          n /// m = n `mod` m == 0
          subs :: [Integer] -> [[Integer]]
          subs = substrings . drop 1

answer = sum $ map (unDigits 10) $ filter predicate pandigitals

main = print answer
