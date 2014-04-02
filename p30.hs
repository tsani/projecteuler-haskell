digits :: Integer -> [Integer]
digits n = map (\x -> (read :: String -> Integer) [x]) (show n)

is5PowSum n = n == (sum $ map (^5) (digits n))
