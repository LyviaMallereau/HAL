{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--
-- EPITECH PROJECT, 2021
-- B-FUN-501-MPL-5-1-HAL-corentin.petrau
-- File description:
-- Error
--

module Error where
import Check
import Control.Exception
import System.Exit

instance Exception ExprError where

data ExprError = NumArgs Integer String
                | TypeMismatch String String
                | BadSpecialForm String
                | NotFunction String
                | UnboundVar String
                | Default

instance Show ExprError where
    show (NumArgs nb func) = "HAL : *** ERROR: NumArgs false : " ++ show nb
                            ++ " on function " ++ func ++ " (abort)"
    show (TypeMismatch ty func) = "HAL : *** ERROR: TypeMismatch invalid : " ++ ty
                            ++ " on function " ++ func ++ " (abort)"
    show (BadSpecialForm func) = "HAL : *** ERROR: BadSpecialForm on func : " ++ func
                                ++ " (abort)"
    show (NotFunction func) = "HAL : *** ERROR: NotFunction on func : " ++ func
                            ++ " (abort)"
    show (UnboundVar func) = "HAL : *** ERROR: UnboundVar on func : " ++ func
                            ++ " (abort)"
    show _ = "HAL : *** ERROR: Generic error (abort)"


handleArith :: (ArithException -> IO a) -> IO a -> IO a
handleArith = handle

handleEOF :: (IOError -> IO a) -> IO a -> IO a
handleEOF = handle

handleAll :: (SomeException -> IO a) -> IO a -> IO a
handleAll = handle