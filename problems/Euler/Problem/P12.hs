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
-- Filename   : P12.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=12
--
-- | * Count divisors.
-- |
-- |   A number n has prime factors p1, p2, ..., pn
-- |
-- |     n = p1 ^ a1 x p2 ^ a2 x ... x pn ^ an
-- |
-- |   And, the number of factors of p1 ^ a1 is (a1 + 1)
-- |
-- |     p1 ^ a1 = 1 x p1 x ..(a1).. x p1
-- |
-- |  Thus, the number of factors of n is
-- |
-- |     (a1 + 1) x (a2 + 1) x .. x (an + 1)
-- |
--------------------------------------------------------------------------------

module Euler.Problem.P12 where

import           Data.List                      ( group )
import           Euler                          ( primeFactors )

nDivisors :: Integer -> Integer
nDivisors = toInteger . product . map ((+ 1) . length) . group . primeFactors

ans :: Integer
ans = head . filter ((> 500) . nDivisors) $ scanl1 (+) [1 ..]
