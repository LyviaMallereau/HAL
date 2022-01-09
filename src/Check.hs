--
-- EPITECH PROJECT, 2021
-- B-FUN-501-MPL-5-1-HAL-corentin.petrau
-- File description:
-- Check
--



module Check where
import Control.Applicative
import Control.Monad
import Data.Fixed
import Debug.Trace
import Control.Exception


data Expr = Atom String
          | List [Expr]
          | DottedList [Expr] Expr
          | Number Integer
          | String String
          | Bool Bool
        --   deriving (Show)

newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where -- <$>
    fmap f p = Parser parser
        where
            parser str = case runParser p str of
                Nothing -> Nothing
                Just (a, str') -> Just (f a, str')


instance Applicative Parser where -- <*>
    pure p = parseIdentity p

    liftA2 f p1 p2 = Parser parser
        where
            parser str = case runParser (parserAnd p1 p2) str of
                Nothing -> Nothing
                Just ((a, b), str') -> Just (f a b, str')

instance Alternative Parser where
    empty = Parser $ const Nothing

    p1 <|> p2 = Parser parser
        where
            -- parser str = case runParser (parserOr p1 p2) str of
            --     Nothing -> Nothing
            --     Just a -> Just a
            parser str = runParser (parserOr p1 p2) str

instance Monad Parser where
    p1 >>= p2 = Parser parser -- >>=
        where
            parser str = case runParser p1 str of
                Nothing -> Nothing
                Just (a, str') -> runParser (p2 a) str'

parserChar :: Char -> Parser Char
parserChar ch = Parser parser
    where
        parser :: String -> Maybe (Char, String)
        parser [] = Nothing
        parser (x:xs)
            | x == ch = Just (x, xs)
            | otherwise = Nothing


parserAnyChar :: String -> Parser Char
parserAnyChar [] = Parser $ const Nothing
parserAnyChar (x:xs) = Parser parser
    where
        parser :: String -> Maybe (Char, String)
        parser [] = Nothing
        parser str = case runParser (parserChar x) str of
                    Just j -> Just j
                    Nothing -> runParser (parserAnyChar xs) str

getAnyChar :: Parser Char
getAnyChar = Parser $ \s -> case s of
    [] -> Nothing
    (c:s) -> Just (c, s)

parseEOF :: Parser ()
parseEOF = Parser  $ \s -> case runParser getAnyChar s of
    Nothing -> Just ((), "")
    Just _ -> Nothing

parserOr :: Parser a -> Parser a -> Parser a
parserOr p1 p2 = Parser parser
    where
        parser str = case runParser p1 str of
                    Just j -> Just j
                    Nothing -> runParser p2 str

parserAnd :: Parser a -> Parser b -> Parser (a, b)
parserAnd p1 p2 = Parser parser
    where
        parser str = case runParser p1 str of
                    Nothing -> Nothing
                    Just (a, str') -> case runParser p2 str' of
                                    Nothing -> Nothing
                                    Just (b, str'') -> Just ((a, b), str'')


parserAndWidth :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parserAndWidth f p1 p2 = Parser parser
    where
        parser str = case runParser (parserAnd p1 p2) str of
            Nothing -> Nothing
            Just ((a, b), str') -> Just (f a b, str')

parseIdentity :: a -> Parser a
parseIdentity a = Parser parser
    where
        parser str = Just (a, str)

parserMany :: Parser a -> Parser [a]
parserMany p = Parser parse
    where
        parse str = case runParser p str of
            Nothing -> Just([], str)
            Just (a, str') -> case runParser (parserMany p) str' of
                Nothing -> Just ([a], str')
                Just (tab, str'') -> Just (a:tab, str'')

parseSome :: Parser a -> Parser [a]
parseSome p = parserAndWidth (:) p (parserMany p)

parseDigit :: Parser Char
parseDigit = parserAnyChar "1234567890"

parseMinus :: Parser String
parseMinus = parserMany (parserAnyChar "-+")

parseUInt :: Parser Int
parseUInt = Parser parse
    where
        parse str = case runParser (parseSome parseDigit) str of
            Nothing -> Nothing
            Just (nb, str') -> Just (read nb, str')


parseDouble :: Parser Double
parseDouble = getSign <$> parseMinus <*> parseUDouble

parseUDouble :: Parser Double
parseUDouble = Parser parse
    where
        parse str = case runParser (parseSome (parseDigit <|> parseSymbol)) str of
            Nothing -> Nothing
            Just (nb, str') -> Just (read nb, str')

-- parseSkipMany :: Parser String
-- parseSkipMany = Parser parse
--     where
--         parse str = case runParser parseSpace str of
--             Nothing -> Just (str, [])
--             Just (nb, str') -> Just (str', nb)

parseMaj :: Parser Char
parseMaj = parserAnyChar "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

parseMin :: Parser Char
parseMin = parserAnyChar "abcdefghijklmnopqrstuvwxyz"

parseLetter :: Parser Char
parseLetter = parseMaj <|> parseMin

parseNbOrLet :: Parser Char
parseNbOrLet = parseDigit <|> parseLetter

parseSymbol :: Parser Char
parseSymbol = parserAnyChar ".!$%&|*+-/:<=?>@^_~#'"

parseSpace :: Parser Char
parseSpace = parserAnyChar " \t\r\n"

parseSpaces :: Parser String
parseSpaces = many parseSpace

skipMany :: String -> String
skipMany [] = []
skipMany (' ':xs) = skipMany xs
skipMany ('\t':xs) = skipMany xs
skipMany ('\n':xs) = skipMany xs
skipMany ('\r':xs) = skipMany xs
skipMany all@(_:xs) = all

getSign :: String -> Double -> Double
getSign [] nb = nb
getSign ('-':xs) nb = getSign xs (nb * (-1))
getSign ('+':xs) nb = getSign xs nb
getSign (_:xs) nb = getSign xs nb
