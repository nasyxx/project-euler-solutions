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
-- Filename   : P23.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=23
--
-- Find the sum of all the positive integers which cannot be written as
-- the sum of two abundant numbers.
--------------------------------------------------------------------------------

module Euler.Problem.P23 where

import           Data.Array                     ( listArray
                                                , (!)
                                                , Array
                                                )
import           Euler.Problem.P21              ( d )

ans :: Integer
ans = sum . filter (not . is2abunds) $ [1 .. limit]

limit :: Integer
limit = 28124

abunds :: [Integer]
-- abunds = filter (\n -> d n > n) [12 .. limit `div` 2]
abunds = filter (abunds' !) [12 .. limit `div` 2]

abunds' :: Array Integer Bool
abunds' = listArray (12, limit) $ map (\n' -> d n' > n') [12 .. limit]

is2abunds :: Integer -> Bool
-- is2abunds n = any (`elem` abunds) . takeWhile (>= 12) . map (n -) $ abunds
-- elem is tooooooo slow.
is2abunds n = any (abunds' !) . takeWhile (>= 12) . map (n -) $ abunds
