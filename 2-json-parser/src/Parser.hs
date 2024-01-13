module Parser where

import Tokenizer
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.Exit
import System.IO

data StringValueExpression = StringValueExpression { getStringValue :: String } deriving (Show, Eq)
data Expression =  NullExpression 
        | BooleanExpression { getBooleanValue :: Bool }
        | NumberExpression { getNumberValue :: Float }
        | StringExpression StringValueExpression 
        | ArrayExpression { getArrayElements :: [Expression] }
        | ObjectExpression { getFields :: [(StringValueExpression, Expression)] }
    deriving (Show, Eq)

removeLeadingAndTrailingSymbols::String -> String
removeLeadingAndTrailingSymbols s = case s of 
    [] -> []
    (_: xs) -> init xs

parseArrayElements :: Tokenizer -> Either String ([Expression], Tokenizer) 
parseArrayElements tokenizer = do 
    let t = eat Whitespace tokenizer
    case lookahead t of 
        Just CloseSquareBracket -> return ([], t)
        Just _ -> do 
            (value, t2) <- parseExpression t
            case lookahead t2 of 
                Just Comma -> do  
                            (values, t3) <- parseArrayElements $ eat Comma t2
                            return ((value: values), t3)
                Just _ -> return ([value], t2)
                Nothing -> Left "there are no tokens!"
        Nothing -> Left "there are no tokens!"

parseArrayExpression :: Tokenizer -> Either String (Expression, Tokenizer) 
parseArrayExpression tokenizer = do
    let t = eat OpenSquareBracket tokenizer
    case lookahead t of 
        Just CloseBracket -> return (ArrayExpression { getArrayElements = [] }, eat CloseSquareBracket t) 
        Just _ -> do 
            (elements, t2) <- parseArrayElements t
            return (ArrayExpression { getArrayElements = elements}, eat CloseSquareBracket t2)
        Nothing -> Left "expected close square bracket"

parsePrimitiveExpression :: Tokenizer -> Either String (Expression, Tokenizer) 
parsePrimitiveExpression tokenizer = case getNextToken tokenizer of 
    Just(Token {getTokenType = NumberType, getTokenValue = value }, t) -> 
        return (NumberExpression { getNumberValue = read(value) }, t)
    Just(Token {getTokenType = StringType, getTokenValue = value }, t) -> 
        return (StringExpression $ StringValueExpression { getStringValue = removeLeadingAndTrailingSymbols value }, t)
    Just(Token {getTokenType = NullType }, t) -> 
        return (NullExpression, t)
    Just(Token {getTokenType = BooleanType, getTokenValue = value }, t) -> 
        return (BooleanExpression { getBooleanValue = value == "true" }, t)
    Just(Token {getTokenType = t}, _) -> Left $ "other types of values are not supported: " ++ show(t)
    Nothing -> Left "there are no tokens but expected at least one"

parseExpression :: Tokenizer -> Either String (Expression, Tokenizer)
parseExpression tokenizer = case lookahead tokenizer of
    Just OpenBracket -> parseObjectExpression tokenizer
    Just OpenSquareBracket -> parseArrayExpression tokenizer
    _ -> parsePrimitiveExpression tokenizer

parseFieldExpression :: Tokenizer -> Either String ((StringValueExpression, Expression), Tokenizer) 
parseFieldExpression tokenizer = do 
    let t1 = eat Whitespace tokenizer
    (key, t2) <- case lookahead t1 of 
        Just StringType -> case getNextToken t1 of 
                Just (Token { getTokenType = StringType, getTokenValue = v }, tk) ->
                    return (StringValueExpression { getStringValue = removeLeadingAndTrailingSymbols v }, tk)
                Just x -> Left $ "expected string token, but " ++ show(x) ++ " token found"
                Nothing -> Left "there are no tokens!"
        Just x -> Left $ "expected string token, but " ++ show(x) ++ " found"
        Nothing -> Left "expected string token, there are no tokens!"
    let t3 =  eat Whitespace $ eat Colon $ eat Whitespace t2 
    (value, t4) <- parseExpression t3
    return ((key, value), eat Whitespace t4)

parseFieldExpressions :: Tokenizer -> Either String ([(StringValueExpression, Expression)], Tokenizer)
parseFieldExpressions tokenizer = do 
    let t1 = eat Whitespace tokenizer
    case lookahead t1 of 
        Just CloseBracket -> return ([], t1)
        Just StringType -> do 
            (field, t2) <- parseFieldExpression t1
            case lookahead t2 of 
                Just Comma -> do 
                    (fields, t3) <- parseFieldExpressions $ eat Comma t2
                    return ((field: fields), t3)
                Just _ -> return ([field], t2)
                Nothing ->  Left "there are no tokens!"
        Just x -> Left $ "expected string token, but " ++ show(x) ++ " found"
        Nothing -> Left "expected } token or string token, but there are no more tokens"

parseObjectExpression :: Tokenizer -> Either String (Expression, Tokenizer)
parseObjectExpression [] = Left "there no tokens for json object" 
parseObjectExpression tokenizer = do 
    let t1 = eat Whitespace $ eat OpenBracket $ eat Whitespace tokenizer
    case lookahead t1 of 
        Just CloseBracket -> return (ObjectExpression { getFields = [] }, eat CloseBracket t1)
        Just StringType -> do 
            (fields, t) <- parseFieldExpressions t1
            return (ObjectExpression { getFields = fields }, eat CloseBracket t)
        Just x -> Left $ "expected string token, but " ++ show(x) ++ " found"
        Nothing -> Left "expected } token or string token, but there are no more tokens"

getObjectExpression :: Tokenizer -> Either String Expression
getObjectExpression tokenizer = do
    let t1 = eat Whitespace tokenizer
    case lookahead t1 of 
        Just OpenBracket -> do
            (e, t2) <- parseObjectExpression tokenizer
            case getNextToken t2 of 
                Nothing -> return e
                _ -> Left $ "there are more tokens than expected: " ++ show(t2)
        Just OpenSquareBracket -> do
            (e, t2) <- parseArrayExpression tokenizer
            case getNextToken t2 of 
                Nothing -> return e
                _ -> Left $ "there are more tokens than expected: " ++ show(t2)
        Just x -> Left $ "expected { or [ tokens, but not " ++ show (x)
        Nothing -> Left "empty string"

readFileFrom :: FilePath -> IO String
readFileFrom filePath = do
    fileExists <- doesFileExist filePath
    if fileExists then 
        readFile filePath
    else 
        exitWithMessage $ "can't read the file: " ++ filePath

validateJson :: String -> Either String Bool
validateJson jsonStr = do 
    tk <- createTokenizer jsonStr
    _ <- getObjectExpression tk
    return True

exitWithMessage :: String -> IO a
exitWithMessage msg = hPutStrLn stderr msg >> exitWith (ExitFailure 2) 

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [] -> exitWithMessage "expected parameter: path to json file"
        (filePath: _) -> do
            jsonStr <- readFileFrom filePath
            case validateJson jsonStr of 
                Left errorMsg -> exitWithMessage errorMsg
                _ -> return () 

