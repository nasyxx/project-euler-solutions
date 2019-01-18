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
import           System.Environment             ( getArgs )
import           Data.Char                      ( isNumber )
--------------------------------------------------------------------------------
import           Euler.Problem                  ( answers
                                                , counts
                                                )
--------------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= putStrLn . parse


parse :: [String] -> String
parse [] =
    unlines
        . map (\(ans, idx) -> "Question " ++ show idx ++ ": " ++ show ans)
        $ zip answers [1 :: Integer ..]
parse ["all"] = parse []
parse args    = unlines . map showAnswer $ args

showAnswer :: String -> String
showAnswer ps
    | all isNumber ps && p <= counts && p > 0 =  desc ps
    ++ show (answers !! (p - 1))
    | otherwise = desc ps ++ "No answer"
  where
    desc ps' = "Question " ++ ps' ++ ": "
    p = read ps :: Int
