{-
 Excited without bugs, have fun ("▔□▔)/hi~♡ Nasy.
 ------------------------------------------------
 |             *         *
 |                  .                .
 |           .
 |     *                      ,
 |                   .
 |
 |                               *
 |          |\___/|
 |          )    -(             .              ·
 |         =\ -   /=
 |           )===(       *
 |          /   - \
 |          |-    |
 |         /   -   \     0.|.0
 |  NASY___\__( (__/_____(\=/)__+1s____________
 |  ______|____) )______|______|______|______|_
 |  ___|______( (____|______|______|______|____
 |  ______|____\_|______|______|______|______|_
 |  ___|______|______|______|______|______|____
 |  ______|______|______|______|______|______|_
 |  ___|______|______|______|______|______|____

There are more things in heaven and earth, Horatio, than are dreamt.
   -- From "Hamlet"
--------------------------------------------------------------------------------

-}

--------------------------------------------------------------------------------
-- |
-- Filename   : Euler.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : LGPL-3.0
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Some useful function for Project Euler problems.
--
-- Include:
--------------------------------------------------------------------------------
-- | Useful functions
--   + @isqrt n@ :: Integral sqrt of n.
--   + @minus l1 l2@ :: The minus/difference of two orderd lists l1 and l2
--   + @union l1 l2@ :: The union of two orderd lists l1 and l2
--------------------------------------------------------------------------------
-- | Primes related functions
--   + @primes@ :: Infinite list of primes.
--   + @primeFactors n@ :: Prime factors of number n
--   + @wheel@ :: The wheel-210 for primes.
--------------------------------------------------------------------------------

module Euler
    ( primes
    , wheel
    , union
    , minus
    , isqrt
    )
where

--------------------------------------------------------------------------------
-- | Infinite list of primes.
-- Using tree merging with wheel to generate infinite list of primes.
-- Reference
--   https://wiki.haskell.org/Prime_numbers#Tree_merging
--   https://wiki.haskell.org/Prime_numbers_miscellaneous#Implicit_Heap
primes :: [Integer]
primes = 2 : 3 : 5 : 7 : r ((11 :) . tail . gaps 11 wheel . merge . roll)
    where r x = x (r x)

-- | Merge parts of results
-- It is equivalent to fold tree with union.
merge :: Ord a => [[a]] -> [a]
merge ~((p : ps) : t) = p : ps `union` merge (pairs t)
    where pairs ~(x : y : t') = x `union` y : pairs t'

-- | Wheel 210
-- loop:
-- 2 4 2 4 6 2 6 4 2 4 6 6 2 6 4 2 6 4 6 8 4 2  4 2
-- 4 8 6 4 6 2 4 6 2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10
wheel :: [Integer]
wheel = cycle $ zipWith (-) =<< tail $ filter ((== 1) . gcd 210) [11 .. 221]

-- | Gaps from wheel.
gaps :: Integral a => a -> [a] -> [a] -> [a]
gaps k ~(w : ws) ~s@(c : cs) | k == c    = gaps (k + w) ws cs
                             | otherwise = k : gaps (k + w) ws s

-- | Roll the wheel to pick out the start point of each primes.
roll :: [Integer] -> [[Integer]]
roll = map roll'  where
    roll' p =
        map (p *) . dropWhile (< p) $ scanl (+) (p - (p - 11) `rem` 210) wheel


--------------------------------------------------------------------------------
-- | Useful functions

-- | Integral sqrt of @n@.
isqrt :: (Integral c, Integral a) => a -> c
isqrt n = floor . sqrt $ (fromIntegral n :: Double)

-- | The minus/difference of two orderd lists.
minus :: Ord a => [a] -> [a] -> [a]
minus (x : xs) (y : ys) = case compare x y of
    LT -> x : minus xs (y : ys)
    EQ -> minus xs ys
    GT -> minus (x : xs) ys
minus xs [] = xs
minus [] xs = xs

-- | The union of two orderd lists.
union :: Ord a => [a] -> [a] -> [a]
union (x : xs) (y : ys) = case compare x y of
    LT -> x : union xs (y : ys)
    EQ -> x : union xs ys
    GT -> y : union (x : xs) ys
union xs [] = xs
union [] ys = ys