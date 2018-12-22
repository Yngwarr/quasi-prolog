module Repl where

import Data.List
import System.IO
import Control.Monad (unless)
import Text.ParserCombinators.Parsec (parse)

import Lib (Term(..), Rule, prove, showSub, squashSub)
import Parser (parseGoals)

read' :: IO (String, Either String [Term])
read' = do
    putStr "?- "
    hFlush stdout
    input <- getLine
    return $ case parse parseGoals "goals" input of
        Left err -> (input, Left (show err))
        Right val -> (input, Right val)

eval' :: [Rule] -> Either String [Term] -> String
eval' _ (Left err) = "Error: " ++ err
eval' knowledge (Right goals) =
    --intercalate "\n" . map showSub $ (prove knowledge goals)
    intercalate "\n" . map showSub $ (map squashSub (prove knowledge goals))

print' :: String -> IO ()
print' = putStrLn

terminate :: [String]
terminate = ["\EOT", ":q", ":quit"]

loop' :: [Rule] -> IO ()
loop' knowledge = do
    (input, goals) <- read'
    unless (input `elem` terminate)
        $ print' (eval' knowledge goals) >> loop' knowledge
