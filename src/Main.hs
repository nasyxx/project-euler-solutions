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
-- License    : GPL-3.0+
--
-- Maintainer : Nasy <nasyxx+haskell@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
--
--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import           System.Environment             ( getArgs )
import           Data.Char                      ( isNumber )
--------------------------------------------------------------------------------
import           Euler.Que
--------------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= putStrLn . parse


parse :: [String] -> String
parse []   = "No question."
parse args = unlines . map showAnswer $ args


showAnswer :: String -> String
showAnswer q | all isNumber q = "Question " ++ q ++ ": " ++ ans q
             | q == "all"     = parse ["1", "2"]
             | otherwise      = "Not a question."


ans :: String -> String
ans "1" = show q1
ans "2" = show q2
ans _   = "No Answer."
