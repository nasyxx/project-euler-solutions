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
-- Filename   : P1.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : LGPL-3.0
-- Copyright  : Nasy © 2019
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=1
--
-- Exhaust the numbers and sum them.
--
-- Maybe there are other simple ways to get the answer, but if we use a
-- computer, for this not too big number, exhaustiveness is also a very
-- convenient method.
--
--------------------------------------------------------------------------------
module Euler.Problem.P1 where

sum_multiples_3_5 :: Integral c => c -> c
sum_multiples_3_5 n =
    sum . filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) $ [1 .. n - 1]

ans :: Integer
ans = sum_multiples_3_5 1000
