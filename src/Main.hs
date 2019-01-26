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
-- Maintainer : Nasy <nasyxx+euler@gmail.com>
--
-- Nasy's Haskell Solutions of Project Euler.
--
-- https://github.com/nasyxx/project-euler-solutions
--
--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import           Control.Monad                  ( mapM )
import           Data.Char                      ( isNumber
                                                , toLower
                                                )
import           System.Environment             ( getArgs )
--------------------------------------------------------------------------------
import           Euler.Problem                  ( answers
                                                , counts
                                                , showAnswer
                                                )
--------------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= parse >>= putStrLn

parse :: [String] -> IO String
parse [] = unlines <$> mapM showAnswer answers
parse args@(a : _)
    | map toLower a == "all"  = parse []
    | all (all isNumber) args = mapM (answer . read) args >>= pure . unlines
    | otherwise               = pure "Not a problem"
  where
    answer n | n < 1 || n > counts =
        pure $ "Problem " ++ show n ++ ": No answer"
    answer n = showAnswer $ answers !! (n - 1)
