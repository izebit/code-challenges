module Parser where

import Tokenizer

data StringValueExpression = StringValueExpression { getStringValue::String } deriving (Show, Eq)
data Expression =  NullExpression 
        | BooleanExpression { getBooleanValue::Bool }
        | NumberExpression { getNumberValue::Float }
        | StringExpression StringValueExpression 
        | ArrayExpression { getArrayElements::[Expression] }
        | ObjectExpression { getFields :: [(StringValueExpression, Expression)] }
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

parseFieldExpression :: Tokenizer -> ((StringValueExpression, Expression), Tokenizer) 
parseFieldExpression tokenizer = let 
    t1 = eat Whitespace tokenizer
    (key, t2) = case lookahead t1 of 
        Just StringType -> case getNextToken t1 of 
                Just (Token { getTokenType = StringType, getTokenValue = v }, tk) -> (StringValueExpression { getStringValue = removeLeadingAndTrailingSymbols v }, tk)
                Just x -> error $ "expected string token, but " ++ show(x) ++ " found"
                Nothing -> error "there are no tokens!"
        Just x -> error $ "expected string token, but " ++ show(x) ++ " found"
        Nothing -> error "there are no tokens!"
    t3 =  eat Whitespace $ eat Colon $ eat Whitespace t2 
    (value, t4) = parseExpression t3
    in ((key, value), eat Whitespace t4)

parseFieldExpressions :: Tokenizer -> ([(StringValueExpression, Expression)], Tokenizer)
parseFieldExpressions tokenizer = let 
    t1 = eat Whitespace tokenizer
    in case lookahead t1 of 
        Just CloseBracket -> ([], t1)
        Just StringType -> let 
            (field, t2) = parseFieldExpression t1
            in case lookahead t2 of 
                Just Comma -> let 
                    (fields, t3) = parseFieldExpressions $ eat Comma t2
                    in ((field: fields), t3)
                Just _ -> ([field], t2)
                Nothing ->  error "there are no tokens!"
        Just x -> error $ "expected string token, but " ++ show(x) ++ " found"
        Nothing -> error "there are no tokens!"

parseObjectExpression :: Tokenizer -> (Expression, Tokenizer)
parseObjectExpression tokenizer = let 
    t1 = eat Whitespace $ eat OpenBracket $ eat Whitespace tokenizer
    in case lookahead t1 of 
        Just CloseBracket -> (ObjectExpression { getFields = [] }, eat CloseBracket t1)
        Just StringType -> let 
            (fields, t) = parseFieldExpressions t1
            in (ObjectExpression { getFields = fields }, eat CloseBracket t)
        Just x -> error $ "expected string token, but " ++ show(x) ++ " found"
        Nothing -> error "there are no tokens!"

getObjectExpression :: Tokenizer -> Maybe (Expression, Tokenizer)
getObjectExpression tokenizer = Just $ parseObjectExpression tokenizer

main :: IO ()
main = do
    return ();

