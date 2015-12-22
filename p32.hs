module Main where

import Data.Digits
import Data.List ( sort, nub )

digits' = digits 10

-- 9876

range = [1234 .. 9999]

splitsFor :: Int -> [(Int, Int)]
splitsFor p
    = filter (\(n, m) -> n * m == p)
             [(n, p `div` n) | n <- [1..floor $ sqrt $ fromIntegral p]]

isPandigital :: (Int, Int) -> Bool
isPandigital (n, m) = [1..9] == (sort $ digits' n ++ digits' m ++ digits' p) where
    p = n * m

answer = filter isPandigital . splitsFor

main
    = print
    $ sum
    $ nub
    $ sort
    $ map (uncurry (*))
    $ filter isPandigital
    $ concatMap splitsFor range
