module Main where

import           Expression
import           Parser

import           Text.Megaparsec
import           Text.Megaparsec.Error

file :: String
file = "P(a)->Q(a)|-P(a)->?aQ(a)\nQ(a)->?aQ(a)\n(Q(a)->?aQ(a))->P(a)->(Q(a)->?aQ(a))\nP(a)->(Q(a)->?aQ(a))\nP(a)->Q(a)\n(P(a)->Q(a))->(P(a)->(Q(a)->?aQ(a)))->(P(a)->?aQ(a))\n(P(a)->(Q(a)->?aQ(a)))->(P(a)->?aQ(a))\nP(a)->?aQ(a)\n"

main :: IO ()
main = do
    -- path <- getLine
    -- file <- readFile path
    case runParser parserFile "" (replaceTurn file) of
        Left parseErr -> putStr $ parseErrorPretty parseErr
        Right _       -> putStrLn "OK"
  where
    replaceTurn :: String -> String
    replaceTurn ('|' : '-' : xxs) = '#' : xxs
    replaceTurn (x : xs) = x : replaceTurn xs
