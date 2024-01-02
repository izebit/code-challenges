{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where
import System.Environment (getArgs)
import System.Exit
import Control.Exception
import Data.Char (isSpace)

data Token = OpenBracket | ClosedBracket | Whitespace deriving (Enum, Show, Eq)
data Element = Object deriving (Enum, Show, Eq)

parse :: [Char] -> Either String [Token]
parse symbols = sequence $ map p symbols where 
    p s | s == '{' = Right OpenBracket
        | s == '}' = Right ClosedBracket
        | isSpace s = Right Whitespace
        | otherwise = Left $ "can't parse symbol: " ++ [s]


constructObject :: [Token] -> Either String (Element, [Token])
constructObject (OpenBracket:ts) = let 
    tokens = dropWhile (== Whitespace) ts
    in case tokens of 
        [] -> Left "there is no closed bracket for json object"
        (ClosedBracket : xs) -> Right (Object, xs)
        _ -> Left "can't parse json object"
constructObject ts = Left $ "can't parse json object: '" ++ (concat $ map show ts) ++ "'"

validate :: Either String (Element, [Token]) -> Either String Element
validate result = case result of 
    Left x -> Left x
    Right (json, []) -> Right json
    Right (_, tokens) -> Left $ "there are unparsed tokens: " ++ show tokens

getInputStringFrom :: [String] -> IO (Either String String)
getInputStringFrom [] = fmap Right getContents
getInputStringFrom (file:_) = do 
    result <- try $ readFile file
    return $ case result of 
       Left (_::IOError) -> Left $ "can't read file: " ++ file
       Right x -> Right x 

errorHandler :: Either String a -> IO a
errorHandler result = case result of 
    Left e -> do 
        putStrLn e
        exitFailure
    Right s -> return s

main :: IO ()
main = do
    args <- getArgs
    inputString <- getInputStringFrom args
    string <- errorHandler inputString
    tokens <- errorHandler $ parse string
    (errorHandler $ validate $ constructObject tokens ) >> return ()

