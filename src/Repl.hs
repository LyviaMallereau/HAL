{-# LANGUAGE BlockArguments #-}
--
-- EPITECH PROJECT, 2021
-- B-FUN-501-MPL-5-1-HAL-corentin.petrau
-- File description:
-- Repl
--


module Repl where
import Debug.Trace
import Control.Monad.IO.Class
import System.IO
import System.Exit
import Control.Exception
import Error
import Expr
import  Parser
import System.IO.Error

runEvalExpr :: String -> IO ()
runEvalExpr [] = return ()
runEvalExpr "quit" = return () 
runEvalExpr str = case readExpr str of
                        Just x -> print x >> runRepl
                        Nothing -> putStrLn "HAL : *** ERROR: Error in the program (abort)" >> exitWith (ExitFailure 84)


runRepl :: IO ()
runRepl = do
        putStr "> "
        hFlush stdout
        input <- handleEOF (\e -> putStrLn "EOF detected, exit HAL" >> exitSuccess) getLine
        handleArith (\e -> putStrLn (displayException e) >> exitWith (ExitFailure 84)) $ runEvalExpr input

--liftIO $ handleAll (\e -> putStrLn (displayException e) >> exitWith (ExitFailure 84)) 