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
-- Filename   : P25.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=25
--
--------------------------------------------------------------------------------

module Euler.Problem.P25 where
import           Euler                          ( fibonaccis )

ans :: Integer
ans = toInteger . length $ takeWhile (< 10 ^ (1000 - 1 :: Integer)) fibonaccis
