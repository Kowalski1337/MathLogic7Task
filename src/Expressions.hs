module Expressions where
  import Data.List

  data Binop = Impl | Conj | Disj | Add | Mul | Equality
    deriving (Eq)

  instance Show Binop where
    show Impl = "->"
    show Conj = "&"
    show Disj = "|"
    show Add = "+"
    show Mul = "*"
    show Equality = "="

  data Quantifier = Any | Exists
    deriving (Eq)
  instance Show Quantifier where
    show Any = "@"
    show Exists = "?"

  data Expression = Binary Binop Expression Expression
                    | Negation Expression
                    | Function String [Expression]
                    | Predicate String [Expression]
                    | Quant Quantifier Expression Expression
                    | Next Expression
                    deriving (Eq)

  instance Show Expression where
    show (Quant q first second) = "(" ++ show q ++ show first ++ show second ++ ")"
    show (Binary op first second) = "(" ++ show first ++ show op ++ show second ++ ")"
    show (Negation expr) = "!" ++ show expr
    show (Next expr) = show expr ++ "\'"
    show (Function f args) = f ++ "(" ++ (intercalate ", " (map show args)) ++ ")"
    show (Predicate f args) = f ++ "(" ++ (intercalate ", " (map show args)) ++ ")"
