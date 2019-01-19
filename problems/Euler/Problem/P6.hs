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
-- Filename   : P6.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : LGPL-3.0
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=6
--
-- s1 = (1 + 2 + .. + n)^2   = (n(n+1))^2
-- s2 = 1^2 + 2^2 + .. + n^2 = n(n + 1)(2n + 1) / 6
--
-- s = s2 - s1 = n^4/4 + n^3/6 - n^2/4 - n/6
--
-- It is not worth to calculate answer with the formulas s in Haskell, due to
-- we have to handle Float and Integer.  The best way I guess is to solve it
-- directly, though I still use Float as the answer to the problem.
--------------------------------------------------------------------------------

module Euler.Problem.P6 where

n :: Num a => a
n = 100

ans :: Float
ans = n ** 4 / 4 + n ** 3 / 6 - n ** 2 / 4 - n / 6


ans' :: Integer
ans' = sum [1 .. n] ^ (2 :: Integer) - sum (map (^ (2 :: Integer)) [1 .. n])
