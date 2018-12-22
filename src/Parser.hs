module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

import Lib (Term(..), Rule)

parseAtom :: Parser Term
parseAtom = do
    first <- lower
    rest <- many (letter <|> digit <|> char '_')
    return $ Atom (first:rest)

parseVar :: Parser Term
parseVar = do
    first <- upper <|> char '_'
    rest <- many (letter <|> digit <|> char '_')
    return $ Var (first:rest)

parseTerm :: Parser Term
parseTerm = do
    Atom name <- parseAtom
    char '('
    body <- parseExpr `sepBy` comma
    char ')'
    return $ Term name body

parseExpr :: Parser Term
parseExpr =
    do
        x <- (try parseTerm) <|> parseAtom
        return x
    <|> parseVar

parseFact :: Parser Rule
parseFact = do
    fact <- parseTerm
    char '.'
    return [fact]

parseRule :: Parser Rule
parseRule = do
    h <- parseTerm
    optSpaces
    string ":-"
    optSpaces
    b <- parseExpr `sepBy` comma
    char '.'
    return (h:b)

parseSentence :: Parser Rule
parseSentence = do
    rule <- (try parseRule) <|> parseFact
    return rule

parseProg :: Parser [Rule]
parseProg = do
    rules <- parseSentence `endBy1` newline
    eof
    return rules

parseGoals :: Parser [Term]
parseGoals = do
    goals <- parseTerm `sepBy1` comma
    char '.'
    return goals

comma :: Parser ()
comma = char ',' >> optSpaces

spaces :: Parser ()
spaces = skipMany1 space

optSpaces :: Parser ()
optSpaces = skipMany space
