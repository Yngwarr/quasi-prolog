module Main where

import System.Environment
import Text.ParserCombinators.Parsec (parse)

import Parser
import Lib

-------- MAIN --------

readProg :: String -> String
readProg input = case parse parseProg "prolog" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found: " ++ show val

main :: IO ()
main = do
    args <- getArgs
    prog <- readFile $ args !! 0
    putStrLn $ readProg prog
