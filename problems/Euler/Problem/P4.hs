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
-- Filename   : P4.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=4
--
-- The simplest way to solve this problem is exhausting all possible numbers.
-- Of course, we can only verify the number above 900, which should contain
-- the result.
--
--------------------------------------------------------------------------------

module Euler.Problem.P4 where

revint :: Integer -> Integer
revint = revint' 0
  where
    revint' r' 0  = r'
    revint' r' n' = revint' (r' * 10 + n_) r_ where (r_, n_) = n' `divMod` 10

ans :: Integer
ans = maximum
    [ x1 * x2
    | x1 <- [900 .. 999]
    , x2 <- [900 .. 999]
    , revint (x1 * x2) == (x1 * x2)
    ]
