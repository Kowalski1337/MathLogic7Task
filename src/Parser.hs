module Parser where
  import Text.Megaparsec
  import qualified Text.Megaparsec.Char.Lexer as L
  import Expressions
  import Data.Void

  type Parser = Parsec Void String

  parserBinary :: Parser Expression
  parserBinary = makeBinaryParser binaryTerm binaryOperators

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
    return Function name args

  parserArgs :: Parser [Expression]
  parserArgs = do
    args <- many parserArg
    return args

  parserArgs :: Parser Expression
  parserArg = do
    arg <- parserFunc
    symbol "," <|> symbol ""
    return arg
