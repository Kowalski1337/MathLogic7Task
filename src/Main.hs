module Main where

import Parser
import Checker

import Text.Megaparsec
import Text.Megaparsec.Error

--file :: String
--file = "P(a)->Q(a)|-P(a)->?aQ(a)\nQ(a)->?aQ(a)\n(Q(a)->?aQ(a))->P(a)->(Q(a)->?aQ(a))\nP(a)->(Q(a)->?aQ(a))\nP(a)->Q(a)\n(P(a)->Q(a))->(P(a)->(Q(a)->?aQ(a)))->(P(a)->?aQ(a))\n(P(a)->(Q(a)->?aQ(a)))->(P(a)->?aQ(a))\nP(a)->?aQ(a)\n"



main :: IO ()
main = do
    -- path <- getLine
    file <- readFile "input.txt"
    case runParser parserFile "" (myReplaceTurn file) of
        Right exprs    -> putStrLn (check exprs)
        Left parseErr -> {-writeFile "output.txt"-} putStrLn " parse error"
  where
    myReplaceTurn :: String -> String
    myReplaceTurn ('|' : '-' : xxs) = '#' : xxs
    myReplaceTurn (x : xs) = x : myReplaceTurn xs
