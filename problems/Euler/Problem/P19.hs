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
-- Filename   : P19.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=19
--
-- How many Sundays fell on the first of the month during the twentieth
-- century (1 Jan 1901 to 31 Dec 2000)?
--
-- We can calculate the first day of week of the next month
-- since 1900.1 as:
--
--   ( 1(Monday) + 31 ) `mod` 7 = 4(Thursday)
--
-- and next is
--   (4 + 28) `mod` 7 = 4
--
-- Thus, we can get the first day of week of all month since 1900.
-- Then we get the answer.
--
-- Besides, there is an easy way to calculate the day of week
--
--   <https://en.wikipedia.org/wiki/Zeller%27s_congruence>
--------------------------------------------------------------------------------

module Euler.Problem.P19 where

ans :: Integer
ans = toInteger . length . filter (== 0) $ drop 12 dayOfWeek

leap :: [Integer]
notleap :: [Integer]
leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
notleap = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

twentiethCentury :: [Integer]
twentiethCentury =
    take (101 * 12) . concat $ notleap : cycle (replicate 3 notleap ++ [leap])

dayOfWeek :: [Integer]
dayOfWeek = scanl (\p n -> (p + n) `mod` 7) 1 twentiethCentury
