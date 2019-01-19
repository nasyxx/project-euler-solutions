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
-- Filename   : Main.hs
-- Project    : nasy-euler
-- Author     : Nasy
-- License    : LGPL-3.0
--
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
--
--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import           Data.Char                      ( isNumber
                                                , toLower
                                                )
import           System.Environment             ( getArgs )
--------------------------------------------------------------------------------
import           Euler.Problem                  ( answers
                                                , counts
                                                )
--------------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= putStrLn . parse

parse :: [String] -> String
parse [] = unlines . map show $ answers
parse args@(a : _)
    | map toLower a == "all"  = parse []
    | all (all isNumber) args = unlines . map (showAnswer . read) $ args
    | otherwise               = "Not a problem"

showAnswer :: Int -> String
showAnswer n | n < 1 || n > counts = "Problem " ++ show n ++ ": No answer"
             | otherwise           = show $ answers !! (n - 1)
