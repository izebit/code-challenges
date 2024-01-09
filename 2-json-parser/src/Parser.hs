module Parser where

import Tokenizer

type FieldExpression = (StringExpression, Expression)
type FieldExpressions = [FieldExpression]
data StringExpression = StringExpression { getStringValue::String } deriving (Show, Eq)
data ArrayExpression = ArrayExpression { getArrayValues::[Expression] } deriving (Show, Eq)

data Expression =  NullValueExpression 
        | BooleanValueExpression { getBooleanValue::Bool }
        | NumberValueExpression { getNumberValue::Float }
        | StringValueExpression StringExpression 
        | ArrayValueExpression ArrayExpression
        | ObjectValueExpression ObjectExpression 
    deriving (Show, Eq)

data ObjectExpression = ObjectExpression {
    getFieldExpressions :: FieldExpressions
} deriving (Show, Eq)

removeLeadingAndTrailingSymbols::String -> String
removeLeadingAndTrailingSymbols s = case s of 
    [] -> []
    (_: xs) -> init xs

parseArrayExpression :: Tokenizer -> (ArrayExpression, Tokenizer) 
parseArrayExpression = undefined

parseExpression :: Tokenizer -> (Expression, Tokenizer)
parseExpression tokenizer = case getNextToken tokenizer of
    Just(Token {getTokenType = NumberType, getTokenValue = value }, t) -> (NumberValueExpression { getNumberValue = read(value) }, t)
    Just(Token {getTokenType = BooleanType, getTokenValue = value }, t) -> (BooleanValueExpression { getBooleanValue = value == "true" }, t)
    Just(Token {getTokenType = NullType}, t) -> (NullValueExpression , t)
    Just(Token {getTokenType = StringType, getTokenValue = value }, t) ->  (StringValueExpression $ StringExpression { getStringValue =  removeLeadingAndTrailingSymbols value }, t)
    _ -> error $ "other types of values are not supported: " ++ show(tokenizer)

parseFieldExpression :: Tokenizer -> (FieldExpression, Tokenizer) 
parseFieldExpression tokenizer = let 
    t = eat Whitespace tokenizer
    (key, t2) = case getNextToken t of 
        Just (Token { getTokenType = StringType, getTokenValue = v }, tk) -> (StringExpression { getStringValue = removeLeadingAndTrailingSymbols v }, tk)
        _ -> error $ "can't parse key" ++ show(t)
    t3 =  eat Whitespace $ eat Colon $ eat Whitespace t2 
    (value, t4) = parseExpression t3
    in ((key, value), eat Whitespace t4)


parseFieldExpressions :: Tokenizer -> (FieldExpressions, Tokenizer)
parseFieldExpressions tokenizer = let 
    (field, t1) = parseFieldExpression $ eat Whitespace tokenizer
    (fields, tk) = case lookahead t1 of 
        Just Comma -> let
            (fs, t2) = parseFieldExpressions $ eat Comma t1
            in ((field: fs), t2)
        _ -> ([field], t1) 
    in (fields, tk)
        

parseObjectExpression :: Tokenizer -> Maybe (ObjectExpression, Tokenizer)
parseObjectExpression tokenizer = let 
    t1 = eat Whitespace $ eat OpenBracket $ eat Whitespace tokenizer
    in case lookahead t1 of 
        Just CloseBracket -> Just ( ObjectExpression { getFieldExpressions = [] }, eat CloseBracket t1)
        Just StringType -> let 
            (fieldExpressions, t) = parseFieldExpressions t1
            in Just ( ObjectExpression { getFieldExpressions = fieldExpressions }, eat CloseBracket t)
        _ -> error $ "can't parse object with next token: " ++ show (tokenizer)



main :: IO ()
main = do
    return ();

