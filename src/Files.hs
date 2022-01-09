--
-- EPITECH PROJECT, 2021
-- B-FUN-501-MPL-5-1-HAL-corentin.petrau
-- File description:
-- Files
--

module Files where
import Control.Applicative
import Control.Monad
import Check
import Parser
import Expr
import Repl

parseAndEvalFile :: String -> Maybe Expr
parseAndEvalFile input = case runParser (many parseExpr <* parseSpaces <* parseEOF) (skipMany input) of
                Just (res, []) ->  evalFile res
                Just (res, str') -> Nothing
                Nothing -> Nothing

evalFile :: [Expr] -> Maybe Expr
evalFile [] = Nothing
evalFile [s] = eval s
evalFile (s:ss) = eval s >> evalFile ss

readFiles :: [FilePath] -> IO (Maybe Expr)
readFiles [] = pure Nothing
readFiles ["-i"] = Nothing <$ runRepl
readFiles [s] = parseAndEvalFile <$> readFile  s
readFiles (s:ss) = parseAndEvalFile <$> readFile s >> readFiles ss