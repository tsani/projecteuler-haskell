module Main where

{-
 - The decimal number, 585 = 10010010012 (binary), is palindromic in both
 - bases.
 - Find the sum of all numbers, less than one million, which are
 - palindromic in base 10 and base 2.
 - (Please note that the palindromic number, in either base, may not include
 - leading zeros.)
 -}

import Data.Digits

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

palindromeBase :: Int -> Int -> Bool
palindromeBase n b = palindrome $ digits b n

answer = sum $ filter (\n -> palindromeBase n 10 && palindromeBase n 2) [1..1000000]

main = print answer
