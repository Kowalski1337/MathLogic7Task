module Main where

import Parser
import Checker

import Text.Megaparsec
import Text.Megaparsec.Error

--file :: String
--file = "P(a)->Q(a)|-P(a)->?aQ(a)\nQ(a)->?aQ(a)\n(Q(a)->?aQ(a))->P(a)->(Q(a)->?aQ(a))\nP(a)->(Q(a)->?aQ(a))\nP(a)->Q(a)\n(P(a)->Q(a))->(P(a)->(Q(a)->?aQ(a)))->(P(a)->?aQ(a))\n(P(a)->(Q(a)->?aQ(a)))->(P(a)->?aQ(a))\nP(a)->?aQ(a)\n"


p :: String
p = "@a@b@c(a=b->a=c->b=c)->@b@c(a+0=b->a+0=c->b=c)"

main :: IO ()
main = do
    -- path <- getLine
    file <- readFile "correct11.in"
    case runParser parserFile "" (myReplaceTurn file) of
        Right exprs   -> putStrLn (check exprs)
        Left parseErr -> {-writeFile "output.txt"-} putStrLn " parse error"
  where
    myReplaceTurn :: String -> String
    myReplaceTurn ('|' : '-' : xxs) = '#' : xxs
    myReplaceTurn (x : xs) = x : myReplaceTurn xs
