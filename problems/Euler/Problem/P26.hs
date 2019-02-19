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
--
-- To find the decimal fraction part of a number, we need to
-- compare the remainders from 10^n divided by it.  For example:
--
-- > 1         / 7 = 0.(142857)
-- > 1         / 7 = 0........1 <--
-- > 10        / 7 = 1........3
-- > 100       / 7 = 14.......2
-- > 1000      / 7 = 142......6
-- > 10000     / 7 = 1428.....4
-- > 100000    / 7 = 14285....5
-- > 1000000   / 7 = 142857...1 <--
-- > 10000000  / 7 = 1428571..3
--
-- The list of remainders is [1,3,2,6,4,5,1,3,..].  To easily find
-- out the recurring cycle of remainders, we can reverse the list.
--
-- > r   rs
-- > 1 : [5,4,6,2,3,1]
--
-- The index of 1 in rs is 5, so the count of the recurring cyclc is 5+1 = 6.
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
remainders n = iterate ((`rem` n) . (* 10)) 1

rcycle :: (Eq a, Num a) => [a] -> [a] -> Int
rcycle _  []        = 0
rcycle _  (0 : _  ) = 0
rcycle rs (r : rs') = case r `elemIndex` rs of
    Just i  -> i + 1
    Nothing -> rcycle (r : rs) rs'
