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
-- Filename   : P27.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=27
--
-- Problem 27:
--
-- Quadratic Expression:
--
-- > n^2 + a*n + b, where |a|<1000 and |b|≤1000
--
-- Find the product of the coefficients, a and b, for the quadratic
-- expression that produces the maximum number of primes for consecutive
-- values of n, starting with n=0.
--
-- The first thing we thought of was to use an isPrime function to
-- calculate the maximum length of given a, b.  This we could easily
-- notice that the b should be a prime, and bigger than |n^2 + a*n|.
--
-- However, when looking at another formula, n^2 - 79*n + 1601, we
-- noticed it could be written as (n-40)^2 + (n-40) + 41, which just
-- contains another same 40 primes as n^2 + n + 41 does.
--
-- Thus, to suit the expression, we take
-- > (n-x)^2 + (n-x) + 41 = n^2 - (2*x-1)*n + (x^2-x+41)
-- > |2*x - 1| < 1000 and |x^2 - x + 41| < 1000
--------------------------------------------------------------------------------

module Euler.Problem.P27 where

ans :: Integer
ans = -(2 * x - 1) * (x ^ (2 :: Integer) - x + 41)
    where x = until (\x' -> x' ^ (2 :: Integer) - x' + 41 > 1000) (+ 1) 1 - 1
