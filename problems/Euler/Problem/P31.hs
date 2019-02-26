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
-- Filename   : P31.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=31
--
-- How many different ways can £2 be made using any number of coins?
--
-- The standard recursive algorithm is tooo slow.  To be faster, we can
-- just count the ways instead of generate all of them.
--------------------------------------------------------------------------------

module Euler.Problem.P31 where

ans :: Integer
ans = ways (length coins) 200

coins :: [Integer]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

-- The standard recursive algorithm is tooooo slow.
-- ways :: (Num p, Integral a) => [a] -> a -> p
-- ways []       0 = 1
-- ways []       _ = 0
-- ways (c : cs) x = sum $ map ways' [0 .. div x c]
--     where ways' k = ways cs (x - k * c)

ways :: Num p => Int -> Integer -> p
ways 1  _    = 1
ways cl rest = sum $ map ways' [0 .. rest `div` coins !! cl']
  where
    cl' = cl - 1
    ways' k = ways cl' (rest - k * coins !! cl')
