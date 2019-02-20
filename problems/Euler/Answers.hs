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
-- Filename   : answers.hs
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

module Euler.Answers
    ( answers
    , counts
    , showAnswer
    )
where

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
import qualified Euler.Problem.P15             as P15
import qualified Euler.Problem.P16             as P16
import qualified Euler.Problem.P17             as P17
import qualified Euler.Problem.P18             as P18
import qualified Euler.Problem.P19             as P19
import qualified Euler.Problem.P20             as P20
import qualified Euler.Problem.P21             as P21
import qualified Euler.Problem.P22             as P22
import qualified Euler.Problem.P23             as P23
import qualified Euler.Problem.P24             as P24
import qualified Euler.Problem.P25             as P25
import qualified Euler.Problem.P26             as P26
import qualified Euler.Problem.P27             as P27

data Answer = I Int Integer | Io Int (IO Integer) | Ios Int (IO String)

showAnswer :: Answer -> IO String
showAnswer (I   p ans) = pure $ "Problem " ++ show p ++ "\t" ++ show ans
showAnswer (Io  p ans) = (("Problem " ++ show p ++ "\t") ++) . show <$> ans
showAnswer (Ios p ans) = (("Problem " ++ show p ++ "\t") ++) <$> ans

set :: Int -> Either Integer (Either (IO Integer) (IO String)) -> Answer
set idx (Left  ans        ) = I idx ans
set idx (Right (Left  ans)) = Io idx ans
set idx (Right (Right ans)) = Ios idx ans

counts :: Int
counts = length answers

answers :: [Answer]
answers = zipWith
    set
    [1 ..]

    [ Left P1.ans
    , Left P2.ans
    , Left P3.ans
    , Left P4.ans
    , Left P5.ans
    , Left $ truncate P6.ans
    , Left P7.ans
    , Left P8.ans
    , Left P9.ans
    , Left P10.ans
    , Left P11.ans
    , Left P12.ans
    , Right $ Right P13.ans
    , Left P14.ans
    , Left P15.ans
    , Left P16.ans
    , Left P17.ans
    , Right $ Left P18.ans
    , Left P19.ans
    , Left P20.ans
    , Left P21.ans
    , Right $ Left P22.ans
    , Left P23.ans
    , Left P24.ans
    , Left P25.ans
    , Left P26.ans
    , Left P27.ans
    ]
