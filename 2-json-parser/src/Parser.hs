module Parser where

import Tokenizer

type FieldExpression = (StringValueExpression, Expression)
type FieldExpressions = [FieldExpression]
data StringValueExpression = StringValueExpression { getStringValue::String } deriving (Show, Eq)

data Expression =  NullExpression 
        | BooleanExpression { getBooleanValue::Bool }
        | NumberExpression { getNumberValue::Float }
        | StringExpression StringValueExpression 
        | ArrayExpression { getArrayElements::[Expression] }
        | ObjectExpression { getFields :: FieldExpressions }
    deriving (Show, Eq)

removeLeadingAndTrailingSymbols::String -> String
removeLeadingAndTrailingSymbols s = case s of 
    [] -> []
    (_: xs) -> init xs

parseArrayElements :: Tokenizer -> ([Expression], Tokenizer) 
parseArrayElements tokenizer = let 
     t = eat Whitespace tokenizer
     in case lookahead t of 
        Just CloseSquareBracket -> ([], t)
        Just _ -> let 
            (value, t2) = parseExpression t
            in case lookahead t2 of 
                Just Comma -> let  
                            (values, t3) = parseArrayElements $ eat Comma t2
                            in ((value: values), t3)
                Just _ -> ([value], t2)
                Nothing -> error "there are no tokens!"
        Nothing -> error "there are no tokens!"

parseArrayExpression :: Tokenizer -> (Expression, Tokenizer) 
parseArrayExpression tokenizer = let
      t = eat OpenSquareBracket tokenizer
      in case lookahead t of 
        Just CloseBracket -> (ArrayExpression { getArrayElements = [] }, eat CloseSquareBracket t) 
        Just _ -> let 
            (elements, t2) = parseArrayElements t
            in (ArrayExpression { getArrayElements = elements}, eat CloseSquareBracket t2)
        Nothing -> error "expected close square bracket"

parsePrimitiveExpression :: Tokenizer -> (Expression, Tokenizer) 
parsePrimitiveExpression tokenizer = case getNextToken tokenizer of 
    Just(Token {getTokenType = NumberType, getTokenValue = value }, t) -> (NumberExpression { getNumberValue = read(value) }, t)
    Just(Token {getTokenType = StringType, getTokenValue = value }, t) -> (StringExpression $ StringValueExpression { getStringValue =  removeLeadingAndTrailingSymbols value }, t)
    Just(Token {getTokenType = NullType }, t) ->  (NullExpression , t)
    Just(Token {getTokenType = BooleanType, getTokenValue = value }, t) -> (BooleanExpression { getBooleanValue = value == "true" }, t)
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
        Just (Token { getTokenType = StringType, getTokenValue = v }, tk) -> (StringValueExpression { getStringValue = removeLeadingAndTrailingSymbols v }, tk)
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
        Just CloseBracket -> (ObjectExpression { getFields = [] }, eat CloseBracket t1)
        Just StringType -> let 
            (fieldExpressions, t) = parseFieldExpressions t1
            in (ObjectExpression { getFields = fieldExpressions }, eat CloseBracket t)
        _ -> error $ "can't parse object with next token: " ++ show (tokenizer)

getObjectExpression :: Tokenizer -> Maybe (Expression, Tokenizer)
getObjectExpression tokenizer = Just $ parseObjectExpression tokenizer

main :: IO ()
main = do
    return ();

