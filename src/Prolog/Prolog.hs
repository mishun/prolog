module Prolog.Prolog
    ( Term(..)
    , Rule(..)
    , PrologResult(..)
    , prolog
    ) where

import Control.Monad (foldM)
import Data.List (intercalate, partition)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)


data Term = Var String | Fn String [Term] deriving Eq

instance Show Term where
    show (Var name)         = name
    show (Fn "nil" [])      = "[]"

    show t@(Fn "cons" [_, _]) =
        let go acc (Fn "cons" [e, r]) = go (e : acc) r
            go acc (Fn "nil" [])      = printf "[%s]" $ intercalate ", " $ map show $ reverse acc
            go acc r                  = printf "[%s | %s]" (intercalate ", " $ map show $ reverse acc) (show r)
        in go [] t

    show (Fn name [])       = name
    show (Fn name args)     = printf "%s(%s)" name $ intercalate ", " $ map show args


data Rule = Rule Term [Term]

instance Show Rule where
    show (Rule left [])   = show left ++ "."
    show (Rule left pars) = printf "%s :- %s." (show left) $ intercalate ", " $ map show pars


occursIn :: String -> Term -> Bool
occursIn x (Fn _ args) = any (occursIn x) args
occursIn x (Var y) = x == y


subst :: [(String, Term)] -> Term -> Term
subst insts (Fn f args) = Fn f (map (subst insts) args)
subst insts tm@(Var y)  = fromMaybe tm $ lookup y insts


unify :: [(String, Term)] -> (Term, Term) -> Either String [(String, Term)]
unify insts (Var x, otherTerm) =
    case lookup x insts of
        Just newTerm -> unify insts (newTerm, otherTerm)
        Nothing      -> augment (x, otherTerm) insts

unify insts (Fn f1 args1, Fn f2 args2) =
   if (f1, length args1) == (f2, length args2)
       then foldM unify insts $ zip args1 args2
       else Left "functions do not match"

unify insts (a, b) = unify insts (b, a)


augment :: (String, Term) -> [(String, Term)] -> Either String [(String, Term)]
augment (v, t) insts =
    let augment1 theta (x, s) =
            if occursIn x s && not (s == Var x)
                then Left "Occurs check"
                else return (x, subst theta s)

        rawAugment p l = do
            lst <- mapM (augment1 [p]) l
            return (p : lst)

    in case subst insts t of
        Var w | w == v     -> return insts
              | w < v      -> rawAugment (v, Var w) insts
              | w > v      -> rawAugment (w, Var v) insts
        t' | occursIn v t' -> Left "Occurs check"
        t'                 -> rawAugment (v, t') insts


expand :: Int -> [Rule] -> [(String, Term)] -> [Term] -> Either String [(String, Term)]
expand n rules insts goals =
    let rename s (Var v)     = Var ("~" ++ v ++ s)
        rename s (Fn f args) = Fn f $ map (rename s) args

        renameRule s (Rule conc asms) = Rule (rename s conc) (map (rename s) asms)

        first _ []      = Left "No rules applicable"
        first f (h : t) =
            case f h of
                Right r -> return r
                Left _  -> first f t

    in first (\ rule ->
            case goals of
                []               -> return insts
                goal : restGoals -> do
                    let Rule conc asms = renameRule (show n) rule
                    insts' <- unify insts (conc, goal)
                    let (loc, glob) = partition (\ (v, _) -> occursIn v conc || any (occursIn v) asms) insts'
                    expand (n + 1) rules glob $ map (subst loc) asms ++ restGoals
        ) rules


data PrologResult = No | Yes [(String, Term)]

instance Show PrologResult where
    show No       = "No"
    show (Yes []) = "Yes"
    show (Yes l)  = printf "Yes { %s }" $ intercalate ", " $ map (\ (s, t) -> printf "%s <- %s" s $ show t) l


prolog :: [Rule] -> Term -> PrologResult
prolog rules goal =
    case expand 0 rules [] [goal] of
        Right insts -> Yes $ filter (flip occursIn goal . fst) insts
        Left _      -> No

