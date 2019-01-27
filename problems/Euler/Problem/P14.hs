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
-- Filename   : P14.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=14
--
--------------------------------------------------------------------------------

module Euler.Problem.P14 where

import           Data.List                      ( maximumBy )
import           Data.Ord                       ( comparing )
import           Data.Array                     ( Array
                                                , assocs
                                                , listArray
                                                , (!)
                                                )

-- | Use List is tooooooooo slow when maximum it.
collatzLenL :: Int -> [Integer]
collatzLenL n = collatzSeq
  where
    collatzSeq = 0 : map (collatz . next) [2 .. n]
    next x | even x    = x `div` 2
           | otherwise = x * 3 + 1
    collatz n' | n' < n    = 1 + collatzSeq !! (n' - 1)
               | otherwise = 1 + (collatz . next) n'


collatzLen :: Integer -> Array Integer Integer
collatzLen n = collatzSeq
  where
    collatzSeq = listArray (1, n) $ 0 : map (collatz . next) [2 .. n]
    next x | even x    = x `div` 2
           | otherwise = x * 3 + 1
    collatz n' | n' < n    = 1 + collatzSeq ! n'
               | otherwise = 1 + (collatz . next) n'


ans :: Integer
ans = fst . maximumBy (comparing snd) . assocs $ collatzLen 1000000
