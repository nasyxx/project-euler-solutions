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
-- Filename   : P26.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=26
--
-- Find the value of d < 1000 for which 1/d contains
-- the longest recurring cycle in its decimal fraction part.
--------------------------------------------------------------------------------

module Euler.Problem.P26 where
import           Data.List                      ( elemIndex
                                                , maximumBy
                                                )
import           Data.Ord                       ( comparing )

ans :: Integer
ans = fst . maximumBy (comparing snd) $ map
    (\i -> (i, rcycle [] $ remainders i))
    [1 .. 999]

remainders :: Integral a => a -> [a]
remainders n = iterate ((`mod` n) . (* 10)) 1

rcycle :: (Eq a, Num a) => [a] -> [a] -> Int
rcycle _  []        = 0
rcycle _  (0 : _  ) = 0
rcycle rs (r : rs') = case r `elemIndex` rs of
    Just i  -> i + 1
    Nothing -> rcycle (r : rs) rs'
