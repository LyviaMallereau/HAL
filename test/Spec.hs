--
-- EPITECH PROJECT, 2021
-- B-FUN-501-MPL-5-1-HAL-corentin.petrau
-- File description:
-- Spec
--


import Test.HUnit
import Control.Monad
import Control.Applicative
import Check
import Expr
import Error
import Parser
import Text.Read.Lex (Number)
import GHC.IO.Exception (assertError)


-----------------------------------------FUNCTION--------------------------------------------------------

takeChar :: Parser Char
takeChar = parserAnyChar "abcdd" >>= parserChar

testNothing :: Parser Double
testNothing = getSign <$> parseMinus <*> parseUDouble

------------------------------------------TESTING--------------------------------------------------------

test1 :: Test
test1 = TestCase (assertEqual "simple compare"
                (Just ('c', "db")) (runParser (parserChar 'c') "cdb"))

test2 :: Test
test2 = TestCase (assertEqual "compare nothing"
                Nothing (runParser (parserChar 'c') "zdb"))

test3 :: Test
test3 = TestCase (assertEqual "only one char"
                (Just ('a', "")) (runParser (parserChar 'a') "a"))

test4 :: Test
test4 = TestCase (assertEqual "Simple compare anyChar"
                (Just ('a', "bc")) (runParser (parserAnyChar "a") "abc"))

test5 :: Test
test5 = TestCase (assertEqual "Complex compare anyChar"
                (Just ('b', "ac")) (runParser (parserAnyChar "adeb") "bac"))

test6 :: Test
test6 = TestCase (assertEqual "Nothing anyChar"
                Nothing (runParser (parserAnyChar "adeb") "xyzgft"))

test7 :: Test
test7 = TestCase (assertEqual "P1 or P2"
                (Just ('a', "bc")) (runParser (parserOr (parserChar 'a') (parserChar 'd')) "abc"))

test8 :: Test
test8 = TestCase (assertEqual "Only P2"
                (Just ('d', "bc")) (runParser (parserOr (parserChar 'a') (parserChar 'd')) "dbc"))

test9 :: Test
test9 = TestCase (assertEqual "Only P1"
                Nothing (runParser (parserOr (parserChar 'a') (parserChar 'z')) "dbc"))

test10 :: Test
test10 = TestCase (assertEqual "P1 and P2"
                (Just (('a', 'b'), "c")) (runParser (parserAnd (parserChar 'a') (parserChar 'b')) "abc"))

test11 :: Test
test11 = TestCase (assertEqual "P1 but not P2"
                Nothing (runParser (parserAnd (parserChar 'a') (parserChar 'b')) "azc"))

test12 :: Test
test12 = TestCase (assertEqual "Nothing"
                Nothing (runParser (parserAnd (parserChar 'a') (parserChar 'b')) "xyz"))

test13 :: Test
test13 = TestCase (assertEqual "Unsigned Double"
                (Just (25.36, "")) (runParser parseUDouble "25.36"))

test14 :: Test
test14 = TestCase (assertEqual "Double ?"
                (Just (-25.36, "")) (runParser parseDouble "-25.36"))

test15 :: Test
test15 = TestCase (assertEqual "Not a number"
                Nothing (runParser parseUDouble "this is a number"))

test16 :: Test
test16 = TestCase (assertEqual "Neg Double"
                (Just (-36.25, "")) (runParser parseDouble "-36.25"))

test17 :: Test
test17 = TestCase (assertEqual "Double"
                (Just (36.25, "")) (runParser parseDouble "36.25"))

test18 :: Test
test18 = TestCase (assertEqual "min Letter"
                (Just ('a', "bcd")) (runParser parseLetter  "abcd"))

test19 :: Test
test19 = TestCase (assertEqual "no Letter"
                Nothing (runParser parseLetter  "1bcd"))

test20 :: Test
test20 = TestCase (assertEqual "Maj Letter"
                (Just ('A', "bcd")) (runParser parseLetter  "Abcd"))

test21 :: Test
test21 = TestCase (assertEqual "Number"
                (Just ('1', "Abcd")) (runParser parseNbOrLet  "1Abcd"))

test22 :: Test
test22 = TestCase (assertEqual "Symbol"
                (Just ('%', "Abcd")) (runParser parseSymbol "%Abcd"))

test23 :: Test
test23 = TestCase (assertEqual "Nothing"
                Nothing (runParser parseSymbol "1Abcd"))

test24 :: Test
test24 = TestCase (assertEqual "Nothing"
                (Just ("lol", "lol")) (runParser (parseIdentity "lol") "lol"))

test25 :: Test
test25 = TestCase (assertEqual "compare nothing"
                Nothing (runParser (parserChar 'c') []))


test26 :: Test
test26 = TestCase (assertEqual ">>="
                Nothing (runParser takeChar "abcd"))

test27 :: Test
test27 = TestCase (assertEqual ">>="
                Nothing (runParser takeChar "zqsf"))

test28 :: Test
test28 = TestCase (assertEqual "Nothing"
                Nothing (runParser testNothing "abcd"))

test29 :: Test
test29 = TestCase (assertEqual "Nothing"
                Nothing (runParser parseEOF "kiwi"))

test30 :: Test
test30 = TestCase (assertEqual "EOF"
                (Just ((), "")) (runParser parseEOF []))

test31 :: Test
test31 = TestCase (assertEqual "Int"
                (Just (12, "")) (runParser parseUInt "12"))

test32 :: Test
test32 = TestCase (assertEqual "Int"
                Nothing (runParser parseUInt "abcd"))

test33 :: Test
test33 = TestCase (assertEqual "Int"
                (Just ('a', "bcd")) (runParser parseNbOrLet "abcd"))

test34 :: Test
test34 = TestCase (assertEqual "space"
                (Just (' ', "abcd")) (runParser parseSpace " abcd"))

test35 :: Test
test35 = TestCase (assertEqual "spaces"
                (Just ("    ", "abcd")) (runParser parseSpaces "    abcd"))

test36 :: Test
test36 = TestCase (assertEqual "getSign"
                32 (getSign "++" 32))

test37 :: Test
test37 = TestCase (assertEqual "getSign"
                35 (getSign "--*--" 35))

test38 :: Test
test38 = TestCase (assertEqual "skipMany"
                "kiwi" (skipMany "      \t\r\nkiwi"))

test39 :: Test
test39 = TestCase (assertEqual "skipMany"
                [] (skipMany []))

test40 :: Test
test40 = TestCase (assertEqual "ManyOne"
                (Just ("a", "bcdef")) (runParser (parserMany $ parserChar 'a') "abcdef"))

test41 :: Test
test41 = TestCase (assertEqual "anyChar"
                (Just ('a', "bcd")) (runParser getAnyChar "abcd"))

------------------------------------------LOADING--------------------------------------------------------


tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2,
                TestLabel "test3" test3, TestLabel "test4" test4,
                TestLabel "test5" test5, TestLabel "test6" test6,
                TestLabel "test7" test7, TestLabel "test8" test8,
                TestLabel "test9" test9, TestLabel "test10" test10,
                TestLabel "test11" test11, TestLabel "test12" test12,
                TestLabel "test13" test13, TestLabel "test14" test14,
                TestLabel "test15" test15, TestLabel "test16" test16,
                TestLabel "test17" test17,TestLabel "test18" test18,
                TestLabel "test19" test19, TestLabel "test20" test20,
                TestLabel "test21" test21, TestLabel "test22" test22,
                TestLabel "test23" test23, TestLabel "test24" test24,
                TestLabel "test25" test25, TestLabel "test26" test26,
                TestLabel "test27" test27, TestLabel "test28" test28,
                TestLabel "test29" test29, TestLabel "test30" test30,
                TestLabel "test31" test31, TestLabel "test32" test32,
                TestLabel "test33" test33, TestLabel "test34" test34,
                TestLabel "test35" test35,TestLabel "test36" test36,
                TestLabel "test37" test37,TestLabel "test38" test38,
                TestLabel "test39" test39, TestLabel "test40" test40,
                TestLabel "test41" test41]

main :: IO Counts
main = do
    runTestTT tests