{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
--
-- EPITECH PROJECT, 2021
-- B-FUN-501-MPL-5-1-HAL-corentin.petrau
-- File description:
-- Expr
--

module Expr where
import Check
import Parser
import Data.Fixed
import Control.Applicative
import Control.Monad
import Data.Maybe
import System.IO
import Data.Bool
import Control.Exception
import Error
import Debug.Trace

instance Show Expr where show = showVal

showVal :: Expr -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [Expr] -> String
unwordsList = unwords . map showVal

apply :: String -> [Expr] -> Expr
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [Expr] -> Expr)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("eq?", eqv),
              (">", numBoolBinop (>)),
              ("<", numBoolBinop (<)),
              ("/=", numBoolBinop (/=)),
              ("<=", numBoolBinop (<=)),
              (">=", numBoolBinop (>=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cons", cons),
              ("cdr", cdr),
              ("atom?", atom)]

boolBinop :: (Expr -> a) -> (a -> a -> Bool) -> [Expr] -> Expr
boolBinop unpacker op args = if length args /= 2
                            then throw (NumArgs 2 "boolBinop")
                            else Bool $ unpacker (head args) `op` unpacker (args!!1)

numBoolBinop :: (Integer -> Integer -> Bool) -> [Expr] -> Expr
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (Maybe String -> Maybe String -> Bool) -> [Expr] -> Expr
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [Expr] -> Expr
boolBoolBinop = boolBinop unpackBool

unpackStr :: Expr -> Maybe String
unpackStr (String s) = return s
unpackStr (Number s) =  return $ show s
unpackStr (Bool s) = return $ show s
unpackStr _ = throw (TypeMismatch "String" "unpackStr")

unpackBool :: Expr -> Bool
unpackBool (Bool b) = b
unpackBool _ = throw (TypeMismatch "Boolean" "unpackBool")

numericBinop :: (Integer -> Integer -> Integer) -> [Expr] -> Expr
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: Expr -> Integer
unpackNum (Number n) = n
unpackNum (String str) = let parsed = reads str in
                        if null parsed
                        then throw (TypeMismatch "Number" "unpackNum")
                        else fst $ head parsed
unpackNum (List [n]) =  unpackNum n
unpackNum (Bool b) = unpackNum $ boolToString b
unpackNum _ = throw (TypeMismatch "Number" "unpackNum")

eval :: Expr -> Maybe Expr
eval val@(String _) = Just val
eval val@(Number _) = Just val
eval val@(Bool _) = Just val
eval (List [Atom "quote", val]) = Just val
eval (List (Atom func : args)) = Just $ apply func $ mapMaybe eval args
eval _ = throw Default


car :: [Expr] -> Expr
car [List (x:xs)] = x
car [DottedList (x:xs) _] = x
car [_] = throw (TypeMismatch "pair" "car")
car _ = throw (NumArgs 1 "car")

cdr :: [Expr] -> Expr
cdr [List (x:xs)] = List xs
cdr [DottedList (_:xs) x] = DottedList xs x
cdr [DottedList [xs] x] = x
cdr [_] = throw (TypeMismatch "pair" "cdr")
cdr _ = throw (NumArgs 1 "cdr")

cons :: [Expr] -> Expr
cons [x1, List []] = List [x1]
cons [x, List xs] = List $ x:xs
cons [x, DottedList xs xlast] = DottedList (x:xs) xlast
cons [x1, x2] = DottedList [x1] x2
cons _ = throw (NumArgs 2 "cons")

eqv :: [Expr] -> Expr
eqv [Bool args1, Bool args2] = Bool $ args1 == args2
eqv [Number args1, Number args2] = Bool $ args1 == args2
eqv [String args1, String args2] = Bool $ args1 == args2
eqv [Atom args1, Atom args2] = Bool $ args1 == args2
eqv [List [], List []] = Bool True
eqv [_, _] = Bool False

atom :: [Expr] -> Expr
atom [Bool b] = Bool True
atom [String str] = Bool True 
atom [Number nb] = Bool True
atom [List []] = Bool True
atom [Atom value] = Bool True
atom _ = Bool False


readExpr :: String -> Maybe Expr
readExpr input = case runParser parseExpr (skipMany input) of
                Just (res, []) -> eval res
                Just (res, str') -> Nothing
                Nothing -> Nothing