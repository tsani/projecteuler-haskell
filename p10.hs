multipleFilter :: Integer -> Integer
multipleFilter lim = multipleFilter' [2..lim] []
    where multipleFilter' :: [Integer] -> [Integer] -> Integer
          multipleFilter' [] ps = sum ps
          multipleFilter' (p:ns) ps 
              | p >= (ceiling . sqrt . fromInteger) lim = p + sum ps + sum ns
              | otherwise = multipleFilter' (filter (\x -> x `mod` p /= 0) ns) (p:ps)

main = print $ multipleFilter 2000000
