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
-- Filename   : P21.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=21
--
-- Evaluate the sum of all the amicable numbers under 10000.
--
-- == Sum divisors
-- A number n has prime factors p1, p2, ..., pn
--
-- > n = p1^a1 * p2^a2 * ... * pn^an
--
-- The sum of divisors of a prime p is 1+p.
--
-- We can calculate the sum of divisors of p ^ a as:
--
-- > s p = 1 + p + p^2 + p^3 + .. + p^a = (p^(a+1) - 1)/(p - 1)
--
-- Thus,
--
-- > s n = (s p1) * (s p2) * .. * (s pn)
--
-- And,
--
-- > d n = s n - n
--------------------------------------------------------------------------------

module Euler.Problem.P21 where
import           Euler                          ( primeFactors )
import           Data.List                      ( group )

ans :: Integer
ans = sum $ filter
    (\n -> d n /= 1 && d n < 10000 && n == d (d n) && n /= d n)
    [2 .. 9999]

s :: (Integral a, Integral b) => a -> b -> a
s p a = (p ^ (a + 1) - 1) `div` (p - 1)


s' :: Integer -> Integer
s' = product . map (\ps@(p : _) -> s p (length ps)) . group . primeFactors

d :: Integer -> Integer
d n = s' n - n
