module Main where

import Data.Function ( on )
import Data.Ord ( comparing )
import Data.List ( groupBy, sort, maximumBy )

type Perimeter = Int
type Pair = (Int, Int)
type Length = Int

main = print $ answer pythagoreanPairs

isPythagorean (i, j) = fromIntegral (floor sqSum) == sqSum where
    sqSum = sqrt $ fromIntegral (i^2 + j^2)

sqrt' = floor . sqrt . fromIntegral

pythagoreanPairs = filter isPythagorean [(i, j) | i <- range, j <- range] where
    range = [1..1000]

perimeter :: Pair -> Perimeter
perimeter (i, j) = (sqrt' $ i^2 + j^2) + i + j

answer
    = snd
    . maximumBy (comparing fst)
    . map (\xs -> (length xs, head xs))
    . groupBy (==)
    . sort
    . filter (< 1000)
    . map perimeter
