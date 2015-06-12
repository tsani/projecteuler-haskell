import Data.Digits
import Data.List ( sort )
import Data.Ratio

denominatorsFor numerator = [(numerator+1)..99]

numerators = [10..98]

allFractions = concatMap (\numerator -> 
                             map (\denominator -> 
                                     (numerator, denominator)) $
                             denominatorsFor numerator) 
                         numerators

isValid (n, d) | n >= d = False
               | head nDigits == 0 && head dDigits == 0 = False
               | any (\n -> n `elem` dDigits) nDigits = True
               | otherwise = False
    where nDigits = sort $ digits 10 $ n
          dDigits = sort $ digits 10 $ d

incorrectSimplify (n, d) | n1 == d1 = (n2, d2)
                         | n1 == d2 = (n2, d1)
                         | n2 == d1 = (n1, d2)
                         | n2 == d2 = (n1, d1)
    where [n1, n2] = digits 10 $ n
          [d1, d2] = digits 10 $ d

validFractions = filter isValid allFractions

isValidIncorrectSimplification (n, d) | d' == 0 = False
                                      | n' == 0 = False
                                      | n % d == n' % d' = True
                                      | otherwise = False
    where (n', d') = incorrectSimplify (n, d)

toRatio (n, d) = n % d

answer = product $ map toRatio $ filter isValidIncorrectSimplification validFractions
