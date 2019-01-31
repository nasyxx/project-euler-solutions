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
-- Filename   : P17.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
-- https://projecteuler.net/problem=17
--
-- * When the number is 0, we write nothing.
--
-- * When the number is below 20, we just write the single word.
--
-- * When the number is between 20 and 100, we write tens digit and ones digit.
--
--     * If the number can be divisible by 10, then its ones digit does not
--       need to write any words.
--
-- * When the number is between 100 and 1000, we write hundred-digit, tens digit
--   and ones digit.
--
--     * Its tens and ones digit can be written as number below 100
--------------------------------------------------------------------------------

module Euler.Problem.P17 where

n1 :: [String]
n1 =
    words
        "_ one two three four five six seven eight nine ten eleven twelve thirteen \
        \fourteen fifteen sixteen seventeen eighteen nineteen"

n10 :: [String]
n10 = words "_ twenty thirty forty fifty sixty seventy eighty ninety"

n100 :: String
n100 = "hundred"

n1000 :: String
n1000 = "onethousand"

toEN :: Int -> String
toEN n
    | 0 < n && n < 20 = n1 !! n
    | 20 <= n && n < 100 = n10 !! (div n 10 - 1) ++ toEN (n `mod` 10)
    | 100 <= n && n < 1000 = toEN (div n 100) ++ n100 ++ if mod n 100 == 0
        then ""
        else "and" ++ toEN (n `mod` 100)
    | n == 1000 = n1000
    | otherwise = ""

ans :: Integer
ans = toInteger . length . concatMap toEN $ [1 .. 1000]
