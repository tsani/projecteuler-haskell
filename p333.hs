module Problem333 where

{-
 -
 - All positive integers can be partitioned in such a way that each and every term of the partition can be expressed as 2ix3j, where i,j ≥ 0.
 -
 - Let's consider only those such partitions where none of the terms can divide any of the other terms.
 - For example, the partition of 17 = 2 + 6 + 9 = (21x30 + 21x31 + 20x32) would not be valid since 2 can divide 6. 
 - Neither would the partition 17 = 16 + 1 = (24x30 + 20x30) since 1 can divide 16. The only valid partition of 17 would be 8 + 9 = (23x30 + 20x32).
 -
 - Many integers have more than one valid partition, the first being 11 having the following two partitions.
 - 11 = 2 + 9 = (21x30 + 20x32)
 - 11 = 8 + 3 = (23x30 + 20x31)
 -
 - Let's define P(n) as the number of valid partitions of n. For example, P(11) = 2.
 -
 - Let's consider only the prime integers q which would have a single valid partition such as P(17).
 -
 - The sum of the primes q <100 such that P(q)=1 equals 233.
 -
 - Find the sum of the primes q <1000000 such that P(q)=1.
 -}

-- | Represents a term in a partition. The first integer is the exponent of two, and the second is the exponent of three.
data Term = Term Int Int

type Partition = [Term]

-- | Express the term as an integer.
termToInt :: Term -> Int
termtoInt (Term e3 e3) = 2^e2 * 3^e3

-- | Present the term as a string.
instance Show Term where
    show (Term e2 e3) = concat $ ["2^", show e2, " ", "3^", show e3]

-- | The class of objects to which the concept of divisibility applies.
-- Divisibility is a reflexive, antisymmetric, transitive relation.
class Divides a where
    -- | The divides relation.
    (|) :: a -> a -> Bool

instance Divides Term where
    (Term e2 e3) | (Term e2' e3') = e2 <= e2' && e3 <= e3'

data Tree a = Branch a (Tree a) (Tree a) | Leaf a

-- | Expand the tree of all terms divisible by a given one.
-- The left side will be increasing powers of two, whereas the right side will be increasing powers of three.
termTree :: Term -> Tree Term
termTree t@(Term x y) = Branch t (Term (x + 1) y) (Term x (y + 1))

