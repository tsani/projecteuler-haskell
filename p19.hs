daysInMonthNormal = zip [1..12] [31,28,31,30,31,30,31,31,30,31,30,31]
daysInMonthLeap   = zip [1..12] [31,29,31,30,31,30,31,31,30,31,30,31]

isLeapYear n  
    | n `mod` 100 == 0 && n `mod` 400 == 0 = True
    | n `mod` 100 == 0 = False
    | n `mod` 4 == 0 = True
    | otherwise = False
    
getNumDaysInYear n = 365 + if isLeapYear n then 1 else 0

monthsUntil :: Int -> [[(Int, Int)]]
monthsUntil n = map (\x -> if isLeapYear x then daysInMonthLeap else daysInMonthNormal) [1900..n]

daysUntil :: Int -> Int
daysUntil n = sum $ map getNumDaysInYear [1900..n]

weekDaysSince1900 :: [Int]
weekDaysSince1900 = map (`mod` 7) [0..]

mkDays :: [(Int, Int)] -> [Int] -> [(Int, [Int])]
mkDays [] _ = []
mkDays ((mn, ds):ms) ss = let (weekdays, rest) = splitAt ds ss 
                          in (mn, weekdays) : (mkDays ms rest)

getDayOfWeek year month day = let (_, days') = mkDays (concat $ monthsUntil year) weekDaysSince1900 !! ((year - 1900) * 12 + month - 1) in days' !! (day - 1)

main = print . length . filter (==6) . map (\g -> g 1) $ [f x | f <- map (\y -> getDayOfWeek y) [1901..2000], x <- [1..12]]
