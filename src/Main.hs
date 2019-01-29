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
import           Data.List                      ( intercalate
                                                , isSuffixOf
                                                , isPrefixOf
                                                )
import           System.Directory               ( doesFileExist
                                                , listDirectory
                                                )
import           System.Environment             ( getArgs )
import           System.Process                 ( callCommand )
--------------------------------------------------------------------------------
import           Euler.Answers                  ( answers
                                                , showAnswer
                                                )
--------------------------------------------------------------------------------
data Command = Ans [String] | New [String] | Update | Unknown String | Empty

data Effect = LIME | NAVY | RED | YELLOW | DEFAULT

counts :: IO Int
counts =
    length

        .   filter (isSuffixOf ".hs")
        .   filter (isPrefixOf "P")
        <$> listDirectory "problems/Euler/Problem/"

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
    "new"    -> New args
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
    ans n = do
        counts' <- counts
        if n < 1 || n > counts'
            then pure . with RED $ "Problem " ++ show n ++ "\tNo answer"
            else showAnswer $ answers !! (n - 1)


execute :: Command -> IO String
execute = \case
    Ans args -> (unlines [header, empty] <>) <$> answer args
    New args -> newParser args
    Update   -> upgrade
    Empty    -> pul [header, empty, cmds, ans, new, update]
    Unknown cmd ->
        pul [header, empty, unknown cmd, empty, cmds, ans, new, update]
  where
    pul :: Applicative f => [String] -> f String
    pul = pure . unlines

    -- parser
    newParser :: [String] -> IO String
    newParser [] = pul
        [ header
        , empty
        , tab ++ with RED "The command new needs to specify a number n"
        ]
    newParser (p : ps)
        | all isNumber p && null ps = doesFileExist path >>= \case
            False ->
                problem p
                    <$> readFile "template/problem.hs"
                    >>= writeFile path
                    >>  pul [header, empty, tab ++ with LIME "New Problem " ++ p]
            True ->
                pul [header, empty, tab ++ with RED "Problem " ++ p ++ " exist"]
        | otherwise = pul
            [header, empty, with RED "The command new only accepts a number n"]
        where path = "problems/Euler/Problem/P" ++ p ++ ".hs"

    upgrade :: IO String
    upgrade =
        readFile "template/answers-header.hs"
            <>  updatePkg
            <>  readFile "template/answers-body.hs"
            >>= updateAns
            >>= writeFile path
            >>  callCommand ("brittany --write-mode inplace " ++ path)
            >>  pul [header, empty, tab ++ with LIME "Updated Answers!"]
        where path = "problems/Euler/Answers.hs"
    -- some strings
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
    new     = concat
        [ tab
        , with LIME "new"
        , tab
        , tab
        , tab
        , with NAVY "New problem with template."
        ]
    update  = concat
        [ tab
        , with LIME "update"
        , tab
        , tab
        , tab
        , with NAVY "Update problem output code."
        ]
    unknown cmd = concat
        [ with NAVY "Unknown command: "
        , with RED  cmd
        , newline
        , newline
        , with LIME "You may like to try:"
        ]


problem :: String -> String -> String
problem n str | '#' `elem` str = before ++ n ++ problem n after
              | otherwise      = str
    where (before, _ : after) = break (== '#') str


updateAns :: String -> IO String
updateAns str = do
    counts' <- counts
    pure
        $  str
        ++ "\n    [ Left P1.ans\n"
        ++ prefix
        ++ (intercalate prefix . map ans) [2 .. counts']
        ++ "]\n"
  where
    prefix = "\n    , "
    ans    = \case
        6  -> "Left $ truncate P6.ans"
        13 -> "Right $ Right P13.ans"
        p  -> "Left P" ++ show p ++ ".ans"


updatePkg :: IO String
updatePkg = do
    counts' <- counts
    pure
        . ("\n" ++)
        . (++ "\n")
        . unlines
        . map
              (\p ->
                  "import qualified Euler.Problem.P"
                      ++ show p
                      ++ " as P"
                      ++ show p
              )
        $ [1 .. counts']
