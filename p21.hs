import Data.List

firstDivisors :: Integer -> [Integer]
firstDivisors n
    | n == 0 = []
    | otherwise = divisors' n 1
        where divisors' :: Integer -> Integer -> [Integer]
              divisors' n d
                  | d > (floor $ sqrt $ fromInteger n) = []
                  | n `mod` d == 0 = d:(divisors' n (d + 1))
                  | otherwise = divisors' n (d + 1)

properDivisors :: Integer -> [Integer]
properDivisors n = let ds = firstDivisors n
                in ds ++ init (reverse $ map (\n' -> n `div` n') ds)

divisorSum :: Integer -> Integer
divisorSum n = sum . properDivisors $ n

getAmicable :: Integer -> Maybe Integer
getAmicable n = let ami = divisorSum n
                in if divisorSum ami == n
                   then Just ami
                   else Nothing

amicables' = flip map [2..10000] filterAmicable
    where filterAmicable n = let ami = getAmicable n
                             in case ami of
                                    -- gotta exclude the perfect numbers!!!!!!!!!
                                    Just a -> if a /= n then [n,a] else []
                                    Nothing -> []

main = print . sum . nub . concat $ amicables'

