import Data.Digits
import Data.List ( permutations )

range = [3..999999]

fact n = product [1..n]

predicate n = n == m
    where m = sum $ map fact $ digits 10 n

answer = sum $ filter predicate range
