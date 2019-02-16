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
-- Filename   : P24.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=24
--
-- What is the millionth lexicographic permutation of the digits
-- 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
--
-- To find out the millionth lexicographic permutation of it, we can
-- calculate the count like this:
--
--   Count 0x..  = 9!
--   Count 1x..  = 9! 0x + 1x       = 2 * 9!
--   Count 2x..  = 9! 0x + 1x + 2x  = 3 * 9! > 10^6
--   Count 20x.. = 8! 0x + 1x + 20x = 2 * 9! + 8!
--   Count 21x..
--   ..
--   Count 26x.. = 8! 0x + 1x + 2[013-6] = 2 * 9! + 6 * 8!
--   ..
--
-- We do not need to generate all the permutations, but calculate the remains.
--------------------------------------------------------------------------------

module Euler.Problem.P24 where
import           Data.List                      ( (\\) )

ans :: Integer
ans = read $ lexic (10 ^ (6 :: Int) - 1) digits

digits :: String
digits = "0123456789"

lexic :: Int -> String -> String
lexic r ds | length ds == 1 = ds
           | otherwise      = ds !! d' : lexic r' (ds \\ show (ds !! d'))
    where (d', r') = r `divMod` product [1 .. length ds - 1]
