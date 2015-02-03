module Problem31 where

{-
 - In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
 -
 -     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
 -
 - It is possible to make £2 in the following way:
 -
 -     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
 -
 - How many different ways can £2 be made using any number of coins?
 -
 -}

import Data.List ( reverse )

data Coin = OneP | TwoP | FiveP | TenP | TwentyP | FiftyP | One | Two deriving (Eq, Ord, Show, Enum, Bounded)

asInt :: Coin -> Int
asInt OneP    = 1
asInt TwoP    = 2
asInt FiveP   = 5
asInt TenP    = 10
asInt TwentyP = 20
asInt FiftyP  = 50
asInt One     = 100
asInt Two     = 200

go :: [Coin] -- ^ The possible coins to build from
   -> Int -- ^ The amount of money remaining to build sums with
   -> Int -- ^ How many sums can be built
go _ 0 = 1 -- we can't build sums if there's no money left
go [] _ = 0 -- we can't build sums if we have no coins left
go (c:cs) remaining = sum $ map (\i -> go cs (remaining - i*c')) [0 .. (remaining `div` c')]
    where c' = asInt c

answer :: Int
answer = go (reverse [OneP .. Two]) 200

main = print answer
