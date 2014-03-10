import qualified Data.List as L
import System.IO

ints :: [[Int]]
ints = [
           [75],
           [95,64],
           [17,47,82],
           [18,35,87,10],
           [20,04,82,47,65],
           [19,01,23,75,03,34],
           [88,02,77,73,07,63,67],
           [99,65,04,28,06,16,70,92],
           [41,41,26,56,83,40,80,70,33],
           [41,48,72,33,47,32,37,16,94,29],
           [53,71,44,65,25,43,91,52,97,51,14],
           [70,11,33,28,77,73,17,78,39,68,17,57],
           [91,71,52,38,17,14,91,43,58,50,27,29,48],
           [63,66,04,68,89,53,67,30,73,16,69,87,40,31],
           [04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]

littleInts :: [[Int]]
littleInts = [
                 [3]
                ,[7,4]
                ,[2,4,6]
                ,[8,5,9,3]
                --,[100,2,2,1,1]
             ]

-- got [75,64,82,87,47,75,7,6,40,32,91,17,91,67,98] as best -- wrong
-- fixed triangularSum, got [75,95,17,35,82,75,7,16,80,37,91,17,91,67,98] as best -- wrong

data BasicTree a = Leaf a
                 | Node a (BasicTree a, BasicTree a)

type Tree = BasicTree Int

type BasicTreePath a = [(BasicTree a, BasicTree a) -> BasicTree a]

type TreePath = BasicTreePath Int

instance Show a => Show (BasicTree a) where
    show (Leaf v) = show v
    show (Node v tp) = show v ++ show tp

instance Functor BasicTree where
    fmap f (Leaf v) = Leaf $ f v
    fmap f (Node v (t1, t2)) = Node (f v) (fmap f t1, fmap f t2)

linearSum :: Tree -> ((Tree, Tree) -> Tree) -> Int
linearSum (Leaf n) _      = n
linearSum (Node v br) nt = v + (linearSum (nt br) nt)

triangularSum :: Tree -> Int
triangularSum (Leaf n)            = n
triangularSum (Node v (lft, rgt)) = v + linearSum lft fst + triangularSum rgt

depthOf :: Tree -> Int
depthOf (Leaf _) = 1
depthOf (Node _ (l, _)) = 1 + depthOf l

-- Doesn't work:
-- The Tree in which to find the path with the biggest value
-- -> The current total sum used in the derivation
-- -> The sequence of integers making up the biggest path.
bestPathIn :: Tree -> Int -> [Int]
bestPathIn (Leaf n) _ = [n]
bestPathIn (Node v (lft,rgt)) total = v : (bestPathIn heavierSide (total - v - linearSum lighterSide direction))
    where direction'  = triangularSum lft >= triangularSum rgt
          (heavierSide, lighterSide) = if direction' then (lft,rgt) else (rgt,lft)
          direction   = if direction' then fst else snd
-- if the triangleSum left is small, then it contains more "weight" and we should go there.

-- Given a Tree, converts a TreePath into a list of Ints by walking the path
getSequenceIn :: TreePath -> Tree -> [Int]
getSequenceIn _ (Leaf n) = [n]
getSequenceIn [] (Node n _) = [n]
getSequenceIn (p:ps) (Node n br) = n : (getSequenceIn ps (p br))
     
mkTreeFrom :: [[Int]] -> Int -> Tree
-- last line, here we need to make leaves
mkTreeFrom (is:[])  x = Leaf (is !! x)
mkTreeFrom (is:iss) x = Node (is !! x) (mkTreeFrom iss x, mkTreeFrom iss (x + 1))

getSubTree :: BasicTree a -> BasicTreePath a -> BasicTree a
getSubTree t [] = t
--getSubTree (Leaf v) ps = error $ "Reached bottom of tree before finishing path. " ++ show v
getSubTree (Leaf v) _ = Leaf v
getSubTree (Node v t) (p:ps) = getSubTree (p t) ps

mkPathTo :: (Int, Int) -> BasicTreePath a
mkPathTo (x, y) = (take (y - x) $ repeat fst) ++ (take x $ repeat snd)

allSame :: Eq a => [a] -> Bool
allSame l = all (==(l !! 0)) l

getAllPathsTo :: (Int, Int) -> [BasicTreePath a]
getAllPathsTo p = let firstPath = mkPathTo p 
                      (x, y)    = p
                  in if x == 0 || x == y
                     then [firstPath] -- there is exactly one path
                     else L.permutations firstPath

findBestPathIn :: Tree -> [TreePath] -> [Int]
findBestPathIn t ps = fst $ maxPair $ map (\x -> (getSequenceIn x t, sum $ getSequenceIn x t)) ps

getAllPaths :: Int -> [BasicTreePath a] -- this is abysmal, don't use it.
getAllPaths 1     = [[fst],[snd]]
getAllPaths depth = let ss = flip map [1..depth - 1] (\x -> (x, depth - x))
                    in concat . map L.permutations $ flip map ss (\(l, r) -> (take l $ repeat fst) ++ (take r $ repeat snd))

getSubTreeI :: BasicTree a -> (Int, Int) -> BasicTree a
getSubTreeI t p = getSubTree t (mkPathTo p)

maxPair :: Ord b => [(a, b)] -> (a, b)
maxPair [] = error "Cannot find maximum in empty list."
maxPair ps = maxPair' ps (ps !! 0)
    where maxPair' [] l = l
          maxPair' ((t', s'):ps) (curMaxTree, curMaxSum) = if s' > curMaxSum 
                                                           then maxPair' ps (t', s')
                                                           else maxPair' ps (curMaxTree, curMaxSum)


bestPath :: Tree -> (Int, Int) -> [Int]
bestPath tree (oX, oY)
    | depthOf tree < 3 = let paths = getAllPaths $ depthOf tree
                          in findBestPathIn tree paths
    | otherwise = let ((xOffset, greatestSubTree), _) = maxPair $ map (\(xOff, x) -> ((xOff, x), triangularSum x)) $ map (\(xOff, path) -> (xOff, getSubTree tree path)) $ map (\(xOff, yOff) -> (xOff, mkPathTo (xOff, yOff))) $ zip [0,1,2] [2,2,2]
                  in (findBestPathIn tree $ 
                      getAllPathsTo (xOffset, 2))
                     ++ tail (bestPath greatestSubTree (oX + xOffset - 1, oY + 2))

bruteMaxSum :: Tree -> [Int]
bruteMaxSum (Leaf n) = [n]
bruteMaxSum (Node n (lft, rgt)) = n : if (sum $ bruteMaxSum lft) > (sum $ bruteMaxSum rgt) then bruteMaxSum lft else bruteMaxSum rgt

mainTree = mkTreeFrom ints 0
littleTree = mkTreeFrom littleInts 0

mkTreeFromStr :: [String] -> Tree
mkTreeFromStr strs = let separatedStrs = map words strs
                         ints' = map (\x -> map (read :: String -> Int) x) separatedStrs
                     in mkTreeFrom ints' 0

readBigTree :: String -> Tree
readBigTree = mkTreeFromStr . lines 

readBigTree' :: IO String
readBigTree' = do
              inh <- openFile "triangle.txt" ReadMode
              contents <- hGetContents inh
              return contents

