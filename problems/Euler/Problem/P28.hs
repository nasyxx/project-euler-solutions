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
-- Filename   : P28.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=28
--
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral.
--
-- > 21 22 23 24 25
-- >              ^
-- > 20  7  8  9 10
-- >           ^
-- > 19  6  1  2 11
-- >        ^
-- > 18  5  4  3 12
-- > 17 16 15 14 13
--
-- As identified by @^@ above, the right up corner of the spiral is always n^2.
-- where n is 1, 3, 5, 7, .., odd.
--
-- Thus, we can easily calculate the number of other corners.
--
-- > |n^2 - 1(n-1), n^2         |
-- > |n^2 - 2(n-1), n^2 - 3(n-1)|
--
-- The sum of a ring is 4*n^2 - 6(n-1) when n > 1, and n in {3,5,7,...}
--
-- The sum of all is 1 + 4 * (1^2 + 3^2 + ... + n ^ 2) - 6 * (0 + 2 + 4 + .. + 2(n - 1))
-- > (4n^3 + 3n^2 + 8n - 9) / 6
--------------------------------------------------------------------------------

module Euler.Problem.P28 where

ans :: Integer
ans =
    (1 +)
        . sum
        . map (\n -> 4 * n ^ (2 :: Integer) - 6 * (n - 1))
        $ [3, 5 .. 1001]
