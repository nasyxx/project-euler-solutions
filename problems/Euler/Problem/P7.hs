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
-- Filename   : P7.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : LGPL-3.0
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=7
--
-- Generate primes.
--
-- The prime in the problem is not really big, we can use a simple
-- sieve but a really complex one.
--
-- BTW, there is an interesting paper here:
-- https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
--------------------------------------------------------------------------------
module Euler.Problem.P7 where

primes :: [Integer]
primes = 2 : sieve [3, 5 ..]
  where
    modulus n =
        map (mod n)
            . takeWhile (<= (floor . sqrt $ (fromIntegral n :: Float)))
            $ primes
    sieve = filter (all (> 0) . modulus)

ans :: Integer
ans = primes !! 10000
