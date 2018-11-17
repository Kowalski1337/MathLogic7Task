module Main where

import Parser
import Checker

import Text.Megaparsec
import Text.Megaparsec.Error

main :: IO ()
main = do
    file <- readFile "incorrect10.in"
    case runParser parserFile "" (replaceTurn file) of
        Right exprs   -> putStrLn (check exprs)
        Left parseErr -> {-writeFile "output.txt"-} putStrLn "parse error : "
  where
    replaceTurn :: String -> String
    replaceTurn ('|' : '-' : xxs) = '#' : xxs
    replaceTurn (x : xs) = x : replaceTurn xs
