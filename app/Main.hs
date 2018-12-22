module Main where

import System.Environment
import Text.ParserCombinators.Parsec (parse)

import Parser
import Repl
import Lib

-------- MAIN --------

readProg :: String -> [Rule]
readProg input = case parse parseProg "prolog" input of
    Left err -> error $ "No match: " ++ show err
    Right val -> val

main :: IO ()
main = do
    args <- getArgs
    prog <- readFile $ args !! 0
    putStrLn prog
    loop' $ readProg prog
