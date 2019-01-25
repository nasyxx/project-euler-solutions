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
-- Filename   : P3.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=3
--
-- To solve this problem, we only need to factorize the number and get
-- the largest one.
--
-- Thus, we can take the number n repeatedly divide out its smallest
-- factor, which must be prime, also odd (except 2).  The last one of its
-- factors we get before is sure the largest prime factor of n.
--
--------------------------------------------------------------------------------

module Euler.Problem.P3 where

primesFactors :: Integral t => t -> [t]
primesFactors 1 = []
primesFactors n | null factors = [n]
                | otherwise    = factors ++ primesFactors (n `div` head factors)
  where
    factors = take 1 . filter ((== 0) . mod n) $ 2 : [3, 5 .. (n `div` 2)]

ans :: Integer
ans = last . primesFactors $ 600851475143
