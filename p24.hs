import Data.List

permute (x:y:[]) = [[x,y],[y,x]]
permute (x:xs)   
    | reverse (x:xs) == sort (x:xs) = [x:xs]
    | otherwise                       = let ((p:ps):pms) = permute xs 
                                        in  map ((:) x) (permute $ concat $ (p:ps):pms) ++ map ((:) p) (permute $ concat $ (x:ps):pms)

pow10 xs = pow10' (reverse xs) 0
    where pow10' [] _     = 0
          pow10' (x:xs) p = x*10^p + pow10' xs (p + 1)
          

main = print $ flip (!!) 999999 $ sort $ permutations "0123456789"
