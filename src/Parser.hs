module Parser where
  import Text.Megaparsec.Expr
  import Text.Megaparsec
  import Text.Megaparsec.Char
  import qualified Text.Megaparsec.Char.Lexer as L
  import Expressions
  import Data.Void
  import Control.Monad

  type Parser = Parsec Void String

  sc :: Parser ()
  sc = L.space empty empty empty

  symbol :: String -> Parser ()
  symbol = void . L.symbol sc

  parens :: Parser a -> Parser a
  parens = between (symbol "(") (symbol ")")

  identifier :: Parser String
  identifier = many alphaNumChar

  parserBinary :: Parser Expression
  parserBinary = makeExprParser binaryTerm binaryOperators

  binaryOperators :: [[Operator Parser Expression]]
  binaryOperators =
    [ [ InfixL (Binary Mul <$ symbol "*") ]
    , [ InfixL (Binary Add <$ symbol "+") ] ]

  binaryTerm :: Parser Expression
  binaryTerm = parens parserFunc <|> parserFunc

  parserFunc :: Parser Expression
  parserFunc = do
    name <- identifier
    args <- parserArgs <|> return []
    return $ Function name args

  parserArgs :: Parser [Expression]
  parserArgs = do
    args <- many parserArg
    return args

  parserArg :: Parser Expression
  parserArg = do
    arg <- parserFunc
    symbol "," <|> symbol ""
    return arg
