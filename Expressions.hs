module Expressions where
  import Data.List

  data Binop = Impl | Conj | Disj | Add | Mul | Equality
  instance Show Binop where
    show Impl = "->"
    show Conj = "&"
    show Disj = "|"
    show Add = "+"
    show Mul = "*"
    show Equality = "="

  data Quantifier = Any | Exists
  instance Show Quantifier where
    show Any = "@"
    show Exists = "?"

  data Expression = Binary Binop Expression Expression
                    | Negation Expression
                    | Variable String
                    | Function String [Expression]
                    | Quant Quantifier Expression Expression
                    | Next Expression

  instance Show Expression where
    show (Quant q first second) = "(" ++ show q ++ show first ++ show second ++ ")"
    show (Binary op first second) = "(" ++ show first ++ show op ++ show second ++ ")"
    show (Negation expr) = "!" ++ show expr ++ ""
    show (Variable s) = s
    show (Next expr) = show expr ++ "\'"
    show (Function f args) = f ++ "(" ++ (intercalate ", " (map show args)) ++ ")"
