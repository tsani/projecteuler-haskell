import Data.Digits

-- Solved by hand, by counting the number of digits that each digit precedes.
-- Then, we list digits in descending order of number of follower digits.

-- sort $ nub $ map (!!1) $ flip filter digitCodes $ \[x,y,z] -> x == 7

digitCodes = map (digits 10) codes

codes = [ 319
        , 680
        , 180
        , 690
        , 129
        , 620
        , 762
        , 689
        , 762
        , 318
        , 368
        , 710
        , 720
        , 710
        , 629
        , 168
        , 160
        , 689
        , 716
        , 731
        , 736
        , 729
        , 316
        , 729
        , 729
        , 710
        , 769
        , 290
        , 719
        , 680
        , 318
        , 389
        , 162
        , 289
        , 162
        , 718
        , 729
        , 319
        , 790
        , 680
        , 890
        , 362
        , 319
        , 760
        , 316
        , 729
        , 380
        , 319
        , 728
        , 716
        ]
