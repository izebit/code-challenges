{-# LANGUAGE QuasiQuotes #-}

module Tokenizer where

import Text.Regex.TDFA
import Text.RawString.QQ

tokens :: [(String, TokenType)]
tokens = [
        ([r|^{|], OpenBracket), 
        ([r|^}|], CloseBracket),         
        ([r|^\[|], OpenSquareBracket),
        ([r|^\]|], CloseSquareBracket),
        ([r|^[[:space:]]+|], Whitespace),
        ([r|^,|], Comma),
        ([r|^:|], Colon),
        ([r|^null|], NullType),
        ([r|^(false|true)|], BooleanType),
        ([r|^-?(0|[1-9][0-9]*)(\.[0-9]*)?([eE][-+]?[0-9]+)?|], NumberType),
        ([r|^"([^\\"[:cntrl:]]|\\["\\bfnrt\/]|(\\u[[:xdigit:]]{4}))*"|], StringType)
    ]

toRegex :: String -> Regex
toRegex = makeRegexOpts defaultCompOpt{multiline=False} defaultExecOpt
(=~+) :: String -> String -> (String, String, String)
(=~+) text pattern = match (toRegex pattern :: Regex) text

data TokenType = OpenBracket | CloseBracket | Whitespace | StringType | Colon | Comma | OpenSquareBracket | CloseSquareBracket | BooleanType | NullType | NumberType
    deriving (Eq, Show)
data Token = Token { getTokenType::TokenType, getTokenValue::String } deriving (Show, Eq)

getTokenFrom :: String -> Either String (Maybe (Token, String))
getTokenFrom str = test tokens str where 
    test :: [(String, TokenType)] -> String -> Either String (Maybe (Token, String))
    test [] s = if null s 
                then return Nothing 
                else Left $ "can't parse token: '" ++ s ++ "'"
    test ((pattern, tokenType) : xs) s = result where 
          (_, value, rest) = s =~+ pattern
          result = if value == "" 
                   then test xs s 
                   else return $ Just (Token {getTokenType = tokenType, getTokenValue = value}, rest)

createTokenizer :: [Char] -> Either String Tokenizer
createTokenizer [] = Right []
createTokenizer str = do
    t <- getTokenFrom str
    case t of 
        Just (token, s) -> do 
            ts <- createTokenizer s
            Right $ (token: ts)  
        Nothing -> if null str then Right []
                   else Left $ "there are unparsed str: " ++ str 


type Tokenizer = [Token] 
getNextToken :: Tokenizer -> Maybe (Token, Tokenizer)
getNextToken [] = Nothing
getNextToken (x: xs) = Just (x, xs)

eat :: TokenType -> Tokenizer -> Either String Tokenizer
eat tokenType tokenizer = let 
    eat_ tp tk = case getNextToken tk of
        Just (token, t) -> if getTokenType token == tp 
                           then return t 
                           else Left $ "expected token type: " ++ show (tp) ++ ", but found: " ++ show (getTokenType token)
        _ -> Left $ "cant eat token type: " ++ show (tokenType) ++ " because there is no tokens"
    -- because whitespace might be nothing https://www.json.org/json-en.html
    in if tokenType == Whitespace 
       then case lookahead tokenizer of 
            Just Whitespace -> eat_ Whitespace tokenizer
            _ -> return tokenizer
       else eat_ tokenType tokenizer


lookahead :: Tokenizer -> Maybe TokenType 
lookahead [] = Nothing
lookahead (token: _) = Just $ getTokenType token   