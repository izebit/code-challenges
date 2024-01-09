module Parser where

import Tokenizer

type FieldExpression = (StringExpression, Expression)
type FieldExpressions = [FieldExpression]
data StringExpression = StringExpression { getStringValue::String } deriving (Show, Eq)

data Expression =  NullValueExpression 
        | BooleanValueExpression { getBooleanValue::Bool }
        | NumberValueExpression { getNumberValue::Float }
        | StringValueExpression StringExpression 
        | ArrayValueExpression { getElements::[Expression]}
        | ArrayExpression { getArrayValues::[Expression] }
        | ObjectExpression { getFieldExpressions :: FieldExpressions }
    deriving (Show, Eq)

removeLeadingAndTrailingSymbols::String -> String
removeLeadingAndTrailingSymbols s = case s of 
    [] -> []
    (_: xs) -> init xs

parseArrayExpression :: Tokenizer -> (Expression, Tokenizer) 
parseArrayExpression = undefined

parsePrimitiveExpression :: Tokenizer -> (Expression, Tokenizer) 
parsePrimitiveExpression tokenizer = case getNextToken tokenizer of 
    Just(Token {getTokenType = NumberType, getTokenValue = value }, t) -> (NumberValueExpression { getNumberValue = read(value) }, t)
    Just(Token {getTokenType = StringType, getTokenValue = value }, t) -> (StringValueExpression $ StringExpression { getStringValue =  removeLeadingAndTrailingSymbols value }, t)
    Just(Token {getTokenType = NullType }, t) ->  (NullValueExpression , t)
    Just(Token {getTokenType = BooleanType, getTokenValue = value }, t) -> (BooleanValueExpression { getBooleanValue = value == "true" }, t)
    Just(Token {getTokenType = t}, _) ->  error $ "other types of values are not supported: " ++ show(t)
    Nothing -> error "there are no tokens but expected at least one"

parseExpression :: Tokenizer -> (Expression, Tokenizer)
parseExpression tokenizer = case lookahead tokenizer of
    Just OpenBracket -> parseObjectExpression tokenizer
    Just OpenSquareBracket -> parseArrayExpression tokenizer
    _ -> parsePrimitiveExpression tokenizer

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

parseObjectExpression :: Tokenizer -> (Expression, Tokenizer)
parseObjectExpression tokenizer = let 
    t1 = eat Whitespace $ eat OpenBracket $ eat Whitespace tokenizer
    in case lookahead t1 of 
        Just CloseBracket -> (ObjectExpression { getFieldExpressions = [] }, eat CloseBracket t1)
        Just StringType -> let 
            (fieldExpressions, t) = parseFieldExpressions t1
            in (ObjectExpression { getFieldExpressions = fieldExpressions }, eat CloseBracket t)
        _ -> error $ "can't parse object with next token: " ++ show (tokenizer)

getObjectExpression :: Tokenizer -> Maybe (Expression, Tokenizer)
getObjectExpression tokenizer = Just $ parseObjectExpression tokenizer

main :: IO ()
main = do
    return ();

