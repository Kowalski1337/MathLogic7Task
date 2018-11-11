module Parser where

import           Expression

import           Data.Void
import           Control.Monad

import           Text.Megaparsec
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

sc :: Parser ()
sc = L.space empty empty empty

symbol :: String -> Parser ()
symbol = void . L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = (:) <$> alphaNumChar <*> many digitChar


parserArgs :: Parser [Expression]
parserArgs = do
    firstArg <- parserExpr
    args <- many parserArg
    return $ firstArg : args
  where
    parserArg :: Parser Expression
    parserArg = do
        symbol ","
        arg <- parserExpr
        return arg

---------------------------- Binary ----------------------------

parserExpr :: Parser Expression
parserExpr = makeExprParser parserSubExpr exprOperators
  where
    exprOperators :: [[Operator Parser Expression]]
    exprOperators =
        [ -- func operators
            [ InfixL (Binary Mul <$ symbol "*")   ]
          , [ InfixL (Binary Add <$ symbol "+")   ]
          -- logic operators
          , [ InfixL (Binary Equal <$ symbol "=") ]
          , [ InfixL (Binary And <$ symbol "&")   ]
          , [ InfixL (Binary Or <$ symbol "|")    ]
          -- impl operator
          , [ InfixR (Binary Impl <$ symbol "->") ]
        ]

---------------------------- Unary ----------------------------

parserSubExpr :: Parser Expression
parserSubExpr = do
    negs <- many (char '!')
    expr <- parens parserExpr <|> parserNamed <|> parserQuant
    nexts <- many (char '\'')
    return $ convertExpr negs nexts expr
  where
    convertExpr :: String -> String -> Expression -> Expression
    convertExpr (ng : ngs) nexts expr = convertExpr ngs nexts (Unary Neg expr)
    convertExpr negs (nxt : nxts) expr = convertExpr negs nxts (Unary Next expr)
    convertExpr _ _ expr = expr


---------------------------- Named -----------------------------

parserNamed :: Parser Expression
parserNamed = do
    name <- identifier
    args <- parens parserArgs <|> return []
    return $ Named name args

---------------------------- Quant -----------------------------

parserQuant :: Parser Expression
parserQuant = parserExist <|> parserAny
  where
    parserAny :: Parser Expression
    parserAny = do
        symbol "@"
        name <- identifier
        expr <- parserSubExpr
        return $ Quant Any name expr

    parserExist :: Parser Expression
    parserExist = do
        symbol "?"
        name <- identifier
        expr <- parserSubExpr
        return $ Quant Exist name expr

---------------------------- File -----------------------------

parserFile :: Parser (([Expression], Expression), [Expression])
parserFile = do
    headerList <- parserArgs
    symbol "|-"
    headerExpr <- parserExpr
    symbol "\n"
    proof <- many parserExprLine
    return ((headerList, headerExpr), proof)
  where
    parserExprLine :: Parser Expression
    parserExprLine = do
        expr <- parserExpr
        symbol "\n"
        return expr
