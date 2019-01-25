{-# LANGUAGE TypeOperators #-}
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
-- Filename   : P9.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=9
--
-- Generate Pythagorean Triplets.
--
--           a^2 +     b^2 = c^2
-- (m^2 - n^2)^2 + (2mn)^2 = (m^2 + n^2)^2
--
--    a + b + c = s
-- => 2 < m < sqrt (s/2)
-- => 1 < n < m - 1
--------------------------------------------------------------------------------

module Euler.Problem.P9 where

triplets :: Integral a => a -> [(a, a, a)]
triplets s = filter
    (\(a, b, c) -> a + b + c == s)
    [ (m * m - n * n, 2 * m * n, m * m + n * n)
    | m <- [2 .. (floor . sqrt $ (fromIntegral s :: Float))]
    , n <- [1 .. (m - 1)]
    ]


ans :: Integer
ans = product . (\(a, b, c) -> [a, b, c]) . head $ triplets 1000
