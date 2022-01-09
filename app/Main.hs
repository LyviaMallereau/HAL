--
-- EPITECH PROJECT, 2021
-- B-FUN-501-MPL-5-1-HAL-corentin.petrau
-- File description:
-- Main
--

module Main where
import System.Exit
import System.Environment
import Debug.Trace
import Control.Exception
import Check
import Expr
import Repl
import Error
import Files

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runRepl
        ["-i"] -> runRepl
        files -> handleAll (\e -> putStrLn (displayException e) >> exitWith (ExitFailure 84)) $ readFiles files >>= maybe (pure ()) print 
