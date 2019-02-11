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
-- Filename   : P18.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=18
--
-- Find the maximum total from top to bottom of the triangle.
--
-- In fact, this question gives a misleading (maybe).
-- There is no difference between the path with the sum from top to bottom
-- and path with the sum from bottom to top.  However, with the path which
-- calculate the sum from bottom to top, we can save a lot of time.
--
-- So, we can do like this:
--
--   a
--   b c
--   d e f
--   g h i j
--
-- * For d, we need to choose the bigger one from g and h. (Assume it is g).
--
-- * For e, we need to choose the bigger one from h and i. (Assume it is i).
--
-- * For f, we need to choose the bigger one from i and j. (Assume it is i).
--
-- Thus, we have:
--
--   a
--   b c
--   (d+g) (e+i) (f+i)
--
--------------------------------------------------------------------------------

module Euler.Problem.P18 where

ans :: IO Integer
ans = head . total . parse <$> readFile "data/P18"

parse :: String -> [[Integer]]
parse = map (map (read :: String -> Integer) . words) . lines

choose :: (Num a, Ord a) => [a] -> [a] -> [a]
choose []       _             = []
choose (x : xs) (y : y' : ys) = x + max y y' : choose xs (y' : ys)
choose _        _             = error "Are you sure numbers are a triangle"

total :: [[Integer]] -> [Integer]
total = foldr1 choose
