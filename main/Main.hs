module Main where

import Control.Monad (forM_)
import Text.Printf (printf)
import Prolog.Prolog
import Prolog.Parser


runProlog :: String -> [String] -> IO ()
runProlog rules goals =
    case parseRules rules of
        Left err -> do
            putStrLn "Error parsing rules:"
            print err

        Right rs -> do
            forM_ goals $ \ goal -> do
                let Right gl = parseTerm goal
                    res = prolog rs gl
                printf "%s => %s\n" (show gl) (show res)


main :: IO ()
main = do
    runProlog
        "male(albert). \
        \male(edward). \
        \female(alice). \
        \female(victoria). \
        \male(fedya). \
        \parents(fedya, victoria, albert). \
        \parents(edward, victoria, albert). \
        \parents(alice, victoria, albert). \
        \sister_of(X, Y) :- female(X), parents(X, M, F), parents(Y, M, F)."

        [ "sister_of(alice, edward)"
        , "sister_of(alice, X)"
        , "sister_of(Y, X)"
        ]

    runProlog
        "append([], L, L). append([H|T], L, [H|A]) :- append(T, L, A). \
        \mid(P, S, X, R) :- append(P, X, T), append(T, S, R)."
        [ "append([a, b], [c], [a, b, c])"
        , "append([a, b], X, [a, b, c, d, e, d])"
        , "append(X, Y, Z)"
        , "append(X, [d | Y], [a, b, c, d, e, f])"
        , "append([a, b], X, X)"
        , "mid([a, b], [e, f], X, [a, b, c, d, e, f])"
        ]

