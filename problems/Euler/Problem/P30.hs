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
-- Filename   : P30.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=30
--
-- Find the sum of all the numbers that can be written as
-- the sum of fifth powers of their digits.
--
-- We can simply get the limit of the numbers, which must smaller than
-- 5 * (9^5)
--
-- Then calculate the answer.
--------------------------------------------------------------------------------

module Euler.Problem.P30 where

import           Data.Char                      ( digitToInt )

limit :: Integer
limit = 9 ^ (5 :: Integer) * 5

want :: Integer -> Bool
want n = n == want' n
  where
    want' = sum . map ((^ (5 :: Integer)) . toInteger . digitToInt) . show

ans :: Integer
ans = sum $ filter want [2 .. limit]
