module Lib where

import Debug.Trace
import Data.List
import Data.Maybe

-------- DATA TYPES --------

data Term =
    Atom String
    | Var String
    | Term String [Term]
    deriving (Eq)

type Rule = [Term]

-------- SHOW --------

instance Show Term where show = showTerm

showTerm :: Term -> String
showTerm (Atom x) = x
showTerm (Var x) = "?" ++ x
showTerm (Term x ts) = intercalate " " $ ["(", x] ++ (map show ts) ++ [")"]

-------- UNIFICATION --------

type Substitution = [(Term, Term)]
true = []

apply :: Substitution -> [Term] -> [Term]
--apply sub terms | trace ("a: sub = " ++ show sub ++ " terms = " ++ show terms) False = undefined
--apply sub terms = map (applyTerm sub) terms
apply sub = map $ applyTerm sub

applyTerm :: Substitution -> Term -> Term
--applyTerm subs term | trace ("aT: subs = " ++ show subs ++ " term = " ++ show term) False = undefined
applyTerm [] x = x
applyTerm _ a@(Atom _) = a
applyTerm ((Var x, t):sub) (Var y)
    | x == y = applyTerm sub t
    | otherwise = applyTerm sub (Var y)
applyTerm subs (Term name ts) = Term name $ apply subs ts

-- TODO occurs check (unify "?x" "+(?x)" must fail, not return [?x, +(?x)])
unify :: Term -> Term -> Maybe Substitution
unify x y | x == y = Just true
unify (Atom x) (Atom y) = Nothing
unify x@(Var _) y = Just [(x, y)]
unify x y@(Var _) = Just [(y, x)]
unify (Term a xs) (Term b ys)
    | a == b = unifyLists xs ys
    | otherwise = Nothing

unifyLists :: [Term] -> [Term] -> Maybe Substitution
unifyLists [] [] = Just true
unifyLists [] _ = Nothing
unifyLists _ [] = Nothing
unifyLists (x:xs) (y:ys) = do
    sub <- unify x y
    sub' <- unifyLists (apply sub xs) (apply sub ys)
    return $ sub ++ sub'

-------- SEARCH --------

prove :: [Rule] -> [Term] -> [Substitution]
prove knowledge goals = seek knowledge goals 1

seek :: [Rule] -> [Term] -> Int -> [Substitution]
seek _ [] _ = [true]
seek knowledge goals i = do
    let knowledge' = rename knowledge i
    (sub, goals') <- branch knowledge' goals
    solution <- seek knowledge goals' $! i+1
    return $ sub ++ solution

branch :: [Rule] -> [Term] -> [(Substitution, [Term])]
branch knowledge (g:gs) = do
    (h:b) <- knowledge
    sub <- maybeToList $ unify g h
    return (sub, apply sub $ b ++ gs)

rename :: [Rule] -> Int -> [Rule]
rename knowledge i = map (ren i) knowledge
    where
        ren i ts = map (r i) ts
        r i (Var name) = case (elemIndex '~' name) of
            Nothing -> Var $ name ++ "~" ++ show i
            Just n -> Var $ (take (n+1) name) ++ show i
        r i (Term name ts) = Term name $ map (r i) ts
        r i t = t

someFunc :: IO ()
someFunc = putStrLn "someFunc"
