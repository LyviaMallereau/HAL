--
-- EPITECH PROJECT, 2021
-- B-FUN-501-MPL-5-1-HAL-corentin.petrau
-- File description:
-- Parser
--

module Parser where
import Check
import Control.Applicative
import Data.Bool (bool)

parseAtom :: Parser Expr
parseAtom = do
        first <- parseSpaces *> parseLetter <|> parserAnyChar "!$%&|*+-/:<=?>@^_~#"
        tail <- parserMany $ parseNbOrLet <|> parseSymbol
        let atom = first:tail
        return $ case atom of
            "#t" -> Bool True
            "#f" -> Bool False
            _ -> Atom atom

parseSepBy1 :: Parser a -> Parser sep -> Parser [a]
parseSepBy1 p sep = (:) <$> p <*> many (sep *> p)

parseSepBy :: Parser a -> Parser sep -> Parser [a]
parseSepBy p sep = parseSepBy1 p sep <|> pure []

between :: Parser a1 -> Parser a2 -> Parser b -> Parser b
between open close p = open *> p <* close

parseEndBy :: Parser a -> Parser b -> Parser [a]
parseEndBy p sep = many (p <* sep)

parseList :: Parser Expr
parseList = List <$> parseSepBy parseExpr parseSpaces

parseDottedList :: Parser Expr
parseDottedList = do
                head <- parseEndBy parseExpr parseSpaces
                tail <- parserChar '.' *> parseSpaces *> parseExpr
                return $ DottedList head tail

parseQuoted :: Parser Expr
parseQuoted = do
            parserChar '\''
            x <- parseExpr
            pure $ List [Atom "quote", x]

parseNumber :: Parser Expr
parseNumber = Number . read <$> some parseDigit

parseString :: Parser Expr
parseString = do
            parserChar '"'
            x <- parserMany $ parseSymbol <|> parseNbOrLet <|> parseSpace
            parserChar '"'
            return $ String x

parseParenthesis :: Parser Expr
parseParenthesis = do
                parserChar '('
                x <- parseDottedList <|> parseList
                parserChar ')'
                return x

boolToString :: Bool -> Expr
boolToString True = String "#t"
boolToString False = String "#f"

parseExpr :: Parser Expr
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseQuoted
            <|> parseParenthesis