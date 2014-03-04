import qualified Data.IntMap as M

-- Find the collatz sequence for a given starting number using a hastable to look up previously determined sequences
collatz' :: (M.IntMap [Int]) -> Int -> (M.IntMap [Int], [Int])
collatz' nm n
    | n == 1 = (nm, [n])
    | otherwise = case M.lookup n nm of
                      Just ns -> (nm, n:ns)
                      Nothing      -> let (nm', ns) = (collatz' nm (if n `mod` 2 == 0 then collatz1 else collatz2))
                                      in (M.insert n ns nm', n:ns)
        where collatz1 = n `div` 2
              collatz2 = 3 * n + 1

-- Poor man's version of mapM, to thread the state of the last stateful computation through to the next one.
mapCollatz :: [Int] -> [(M.IntMap [Int], [Int])]
mapCollatz ns = mapCollatz' (M.fromList []) ns
    where mapCollatz' :: M.IntMap [Int] -> [Int] -> [(M.IntMap [Int], [Int])]
          mapCollatz' _ []      = []
          mapCollatz' nm (n:ns) = let (nm', ns') = collatz' nm n
                                  in (nm', ns'):(mapCollatz' nm' ns)

-- Find the longest list in a list of lists.
maxLen :: [[a]] -> [a]
maxLen xs = maxLen' xs ([], 0)
    where maxLen' :: [[a]] -> ([a], Int) -> [a]
          maxLen' [] (xs, l) = xs
          maxLen' (p:ps) (xs, l)
              | length p > l = maxLen' ps (p, length p)
              | otherwise    = maxLen' ps (xs, l)

main = print $ flip (!!) 0 $ maxLen . map (\(nm, ns) -> ns) $ mapCollatz [2..1000000]
