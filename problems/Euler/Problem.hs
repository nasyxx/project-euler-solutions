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
-- Filename   : Problem.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
--
--------------------------------------------------------------------------------

module Euler.Problem where

import qualified Euler.Problem.P1              as P1
import qualified Euler.Problem.P2              as P2
import qualified Euler.Problem.P3              as P3
import qualified Euler.Problem.P4              as P4
import qualified Euler.Problem.P5              as P5
import qualified Euler.Problem.P6              as P6
import qualified Euler.Problem.P7              as P7
import qualified Euler.Problem.P8              as P8
import qualified Euler.Problem.P9              as P9
import qualified Euler.Problem.P10             as P10
import qualified Euler.Problem.P11             as P11
import qualified Euler.Problem.P12             as P12
import qualified Euler.Problem.P13             as P13
import qualified Euler.Problem.P14             as P14


data Answer = I Int Integer | Io Int (IO Integer) | Ios Int (IO String)

showAnswer :: Answer -> IO String
showAnswer (I   p ans) = pure $ "Problem " ++ show p ++ "\t:\t" ++ show ans
showAnswer (Io  p ans) = (("Problem " ++ show p ++ "\t:\t") ++) . show <$> ans
showAnswer (Ios p ans) = (("Problem " ++ show p ++ "\t:\t") ++) <$> ans

answers :: [Answer]
answers = zipWith set [1 ..] $ reverse
    [ Left P14.ans
    , Right $ Right P13.ans
    , Left P12.ans
    , Left P11.ans
    , Left P10.ans
    , Left P9.ans
    , Left P8.ans
    , Left P7.ans
    , Left $ truncate P6.ans
    , Left P5.ans
    , Left P4.ans
    , Left P3.ans
    , Left P2.ans
    , Left P1.ans
    ]
  where
    set idx (Left  ans        ) = I idx ans
    set idx (Right (Left  ans)) = Io idx ans
    set idx (Right (Right ans)) = Ios idx ans

counts :: Int
counts = length answers
