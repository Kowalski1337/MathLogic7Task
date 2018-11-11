module Expression where

import Data.List


data BinaryType = Impl | And | Or | Add | Mul | Equal
    deriving Eq

instance Show BinaryType where
    show Impl  = "->"
    show And   = "&"
    show Or    = "|"
    show Add   = "+"
    show Mul   = "*"
    show Equal = "="


data UnaryType = Next | Neg
    deriving Eq

instance Show UnaryType where
    show Next = "\'"
    show Neg  = "!"


data QuantType = Any | Exist
    deriving Eq

instance Show QuantType where
    show Any   = "@"
    show Exist = "?"


data Expression = Named String [Expression]
                | Unary UnaryType Expression
                | Binary BinaryType Expression Expression
                | Quant QuantType String Expression
    deriving Eq

instance Show Expression where
    show (Named name [])           = name
    show (Named name args)         = name ++ "(" ++ (intercalate "," (map show args)) ++ ")"
    show (Unary unT expr)          = if unT == Neg then show unT ++ show expr else show expr ++ show unT
    show (Binary binT expr1 expr2) = "(" ++ show expr1 ++ show binT ++ show expr2 ++ ")"
    show (Quant qT var expr)       = "(" ++ show qT ++ var ++ show expr ++ ")"
