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
{-# LANGUAGE LambdaCase #-}
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
data Command = Ans [String] | New String | Update | Unknown String | Empty

data Effect = LIME | NAVY | RED | YELLOW | DEFAULT

main :: IO ()
main = getArgs >>= execute . parse >>= putStrLn

with :: Effect -> String -> String
with = \case
    LIME    -> ("\x1b[1;92m" ++) . (++ "\x1b[0m")
    NAVY    -> ("\x1b[0;34m" ++) . (++ "\x1b[0m")
    RED     -> ("\x1b[0;31m" ++) . (++ "\x1b[0m")
    YELLOW  -> ("\x1b[1;33m" ++) . (++ "\x1b[0m")
    DEFAULT -> ("\x1b[0m" ++) . (++ "\x1b[0m")


parse :: [String] -> Command
parse (cmd : args) = case cmd of
    "ans"    -> Ans args
    "new"    -> New $ head args
    "update" -> Update
    _        -> Unknown cmd
parse []           = Empty


answer :: [String] -> IO String
answer [] = with LIME . unlines <$> mapM showAnswer answers
answer args@(a : _)
    | map toLower a == "all"  = answer []
    | all (all isNumber) args = with LIME . unlines <$> mapM (ans . read) args
    | otherwise               = pure $ with RED "Not a problem"
  where
    ans n | n < 1 || n > counts =
        pure . with RED $ "Problem " ++ show n ++ "\tNo answer"
    ans n = showAnswer $ answers !! (n - 1)


execute :: Command -> IO String
execute = \case
    Ans args -> (unlines [header, empty] <>) <$> answer args
    New _    -> pure "WIP"
    Update   -> pure "WIP"
    Empty    -> pure $ unlines [header, empty, cmds, ans, new]
    Unknown cmd ->
        pure $ unlines [header, empty, unknown cmd, empty, cmds, ans]
  where
    tab     = "\t"
    newline = "\n"
    empty   = ""
    header  = with YELLOW "Nasy's Project Solutions"
    cmds    = with YELLOW "COMMANDS:"
    ans     = concat
        [ tab
        , with LIME "ans [<Problem n>]"
        , tab
        , with NAVY "Answer of problem n (Show all answers if left empty)"
        ]
    new =
        concat
            [ tab
            , with LIME "new"
            , tab
            , tab
            , with NAVY "New problem with template."
            ]
    unknown cmd = concat
        [ with NAVY "Unknown command: "
        , with RED  cmd
        , newline
        , newline
        , with LIME "You may like to try:"
        ]
