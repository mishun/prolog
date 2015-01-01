module Prolog.Parser
    ( term
    , rule
    , parseRules
    , parseTerm
    ) where

import Data.Char (isUpper)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token
import Prolog.Prolog (Term(..), Rule(..))


prolog :: LanguageDef st
prolog = emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "%"
    , nestedComments  = True
    , identStart      = letter
    , identLetter     = alphaNum <|> oneOf "_"
    , opStart         = opLetter prolog
    , opLetter        = oneOf ":=-|"
    , reservedOpNames = [":-", ",", ".", "|"]
    , reservedNames   = ["cons", "nil"]
    , caseSensitive   = True
    }


prologT :: TokenParser st
prologT = makeTokenParser prolog


term :: Parser Term
term = choice
    [ do
        name @ (u : _) <- identifier prologT
        if isUpper u
            then return $! Var name 
            else do
                args <- option [] $ parens prologT $ term `sepBy` comma prologT
                return $! Fn name args

    , brackets prologT $ do
        terms <- term `sepBy` comma prologT
        tl <- option (Fn "nil" []) $ do
            _ <- symbol prologT "|"
            term
        return $! foldr (\ e t -> Fn "cons" [e, t]) tl terms
    ]


rule :: Parser Rule
rule = do
    left <- term
    right <- option [] $ do
        _ <- symbol prologT ":-"
        term `sepBy` comma prologT
    _ <- symbol prologT "."
    return $! Rule left right


parseRules :: String -> Either ParseError [Rule]
parseRules s = parse (many rule) s s


parseTerm :: String -> Either ParseError Term
parseTerm s = parse term s s

