module Tokenizer where

import Text.Regex.TDFA

tokens :: [(String, TokenType)]
tokens = [
        ("^{", OpenBracket), 
        ("^}", CloseBracket),
        ("^\\[", OpenSquareBracket), 
        ("^\\]", CloseSquareBracket),
        ("^[[:space:]]+", Whitespace),
        ("^,", Comma),
        ("^:", Colon),
        ("^null", NullType),
        ("^(false|true)", BooleanType),
        ("^-?(0|([[:digit:]]+))(\\.[[:digit:]])?([E|e][+-]?[[:digit:]]+)?", NumberType),
        ("^\"([[:word:]|[:space:]]+)\"", StringType)
    ]

data TokenType = OpenBracket | CloseBracket | Whitespace | StringType | Colon | Comma | OpenSquareBracket | CloseSquareBracket | BooleanType | NullType | NumberType
    deriving (Eq, Show)
data Token = Token { getTokenType::TokenType, getTokenValue::String } deriving (Show, Eq)

getTokenFrom :: String -> Maybe (Token, String)
getTokenFrom str = test tokens str where 
    test :: [(String, TokenType)] -> String -> Maybe (Token, String)
    test [] s = if null s 
                then Nothing 
                else error $ "can't parse token: '" ++ s ++ "' "
    test ((pattern, tokenType) : xs) s = result where 
          (_, value, rest) = s =~ pattern :: (String, String, String)
          result = if value == "" 
                   then test xs s 
                   else Just (Token {getTokenType = tokenType, getTokenValue = value}, rest)

createTokenizer :: [Char] -> Tokenizer
createTokenizer [] = []
createTokenizer str = case getTokenFrom str of
    Just (token, rest) -> (token : createTokenizer rest)
    _ -> []


type Tokenizer = [Token] 
getNextToken :: Tokenizer -> Maybe (Token, Tokenizer)
getNextToken [] = Nothing
getNextToken (x: xs) = Just (x, xs)

eat :: TokenType -> Tokenizer -> Tokenizer
eat tokenType tokenizer = let 
    eat_ tp tk = case getNextToken tk of
        Just (token, t) -> if getTokenType token == tp 
                        then t 
                        else error $ "expected token type: " ++ show (tp) ++ ", but found: " ++ show (getTokenType token)
        _ -> error $ "cant eat token type: " ++ show (tokenType) ++ " because there is no tokens"
    -- because whitespace might be nothing https://www.json.org/json-en.html
    in if tokenType == Whitespace 
       then case lookahead tokenizer of 
            Just Whitespace -> eat_ Whitespace tokenizer
            _ -> tokenizer
       else eat_ tokenType tokenizer


lookahead :: Tokenizer -> Maybe TokenType 
lookahead [] = Nothing
lookahead (token: _) = Just $ getTokenType token   