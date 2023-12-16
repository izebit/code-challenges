{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import qualified Data.Text.Encoding as TSE
import qualified Data.ByteString as B
import Data.Text (singleton)
import Data.Char (isSpace)
import System.Environment (getArgs)
import Text.Printf (printf)

data CounterType = BytesCounterType | SymbolsCounterType | WordsCounterType | LinesCounterType deriving (Show, Eq, Enum) 
data Counter = SimpleCounter { getType::CounterType, getResult::Int } | 
               WordsCounter { getType::CounterType, getResult::Int, getIntemediateResult::Int, isWord::Bool }

defaultCounterTypes :: [CounterType]
defaultCounterTypes = [LinesCounterType, WordsCounterType, BytesCounterType]

handleSymbol:: Char -> [Counter] -> [Counter]
handleSymbol symbol counters = map f counters where 
    f counter = case counter of 
        SimpleCounter { getType = SymbolsCounterType, getResult = resultValue } -> counter { getResult = resultValue + 1 }
        SimpleCounter { getType = BytesCounterType, getResult = resultValue } -> counter { getResult = resultValue + increment } where
            text = singleton symbol
            byteString = TSE.encodeUtf8 text
            increment = B.length byteString 
        SimpleCounter {getType = LinesCounterType, getResult = resultValue } -> counter { getResult = resultValue + increment } where
            increment = if symbol == '\n' then 1 else 0
        WordsCounter { getIntemediateResult = intermediateResultValue, isWord = isWordValue } -> updatedCounter where
            isSpaceSymbol = isSpace symbol
            updatedIntermediateResultValue = intermediateResultValue + if isSpaceSymbol && isWordValue then 1 else 0
            updatedIsWordValue = not isSpaceSymbol
            updatedResultValue = updatedIntermediateResultValue + if updatedIsWordValue then 1 else 0
            updatedCounter = counter { isWord = updatedIsWordValue, getIntemediateResult = updatedIntermediateResultValue, getResult = updatedResultValue }
        _ -> undefined

createCounters :: [CounterType] -> [Counter]
createCounters counterTypes = map f counterTypes where 
    f t = case t of 
        WordsCounterType -> WordsCounter { getType = t, getResult = 0, isWord = False, getIntemediateResult = 0 }
        _ ->  SimpleCounter { getType = t, getResult = 0 }

handleString :: [Char] -> [CounterType] -> [(CounterType, Int)]
handleString chars counterTypes = let
    counters = createCounters counterTypes
    handledCounters = foldr handleSymbol counters chars 
    results = map (\c -> (getType c, getResult c)) handledCounters
    in results

getCounterTypesFrom :: String -> Maybe [CounterType]
getCounterTypesFrom args = case args of 
    ('-' : types) -> sequence $ map f types where 
        f s = case s of 
            'c' -> Just BytesCounterType
            'w' -> Just WordsCounterType
            'l' -> Just LinesCounterType
            'm' -> Just SymbolsCounterType
            _   -> Nothing
    _ -> Nothing 

data InputType = Console | File deriving (Enum, Eq, Show)
data InputParamereter = InputParamereter { getInputType:: InputType, getFilePath::Maybe String, getCounterTypes::[CounterType]} deriving Show
getInputParameterFrom :: [String] -> Either InputParamereter String 
getInputParameterFrom args = case args of 
    [] -> Left InputParamereter { getInputType = Console, getFilePath = Nothing, getCounterTypes = defaultCounterTypes }
    [firstArgument] -> Left inputParameter where 
                        inputParameter = case getCounterTypesFrom firstArgument of
                            Just counterTypes -> InputParamereter { getInputType = Console, getFilePath = Nothing, getCounterTypes = counterTypes }
                            Nothing -> InputParamereter { getInputType = File, getFilePath = Just firstArgument, getCounterTypes = defaultCounterTypes }
    (firstArgument : secondArgument : []) -> case getCounterTypesFrom firstArgument of
                                                Just counterTypes -> Left InputParamereter { getInputType = File, getFilePath = Just secondArgument, getCounterTypes = counterTypes }
                                                Nothing -> Right "the first argument must be counter types" 
    _ -> Right "there must be not more than 2 arguments"

getInputStringFrom :: InputType -> Maybe String -> IO String
getInputStringFrom inputType filePath = case inputType of 
    File -> case filePath of
        Just f -> readFile f
        _ -> undefined
    Console -> getContents


getOutputString :: [(CounterType, Int)] -> Maybe String -> String
getOutputString results filePath = let 
    resultTokens = map (\pair -> printf "%8d" $ snd pair) results
    output = foldr (\t s ->  t ++ "" ++ s) "" resultTokens
    fileSuffix = maybe "" (\s -> " " ++ s) filePath
    in output ++ fileSuffix


main :: IO ()
main = do 
    args <- getArgs
    let inputParameter = getInputParameterFrom args
    case inputParameter of 
        Left InputParamereter { getInputType = inputType, getFilePath = filePath, getCounterTypes = counterTypes } -> do 
            content <- getInputStringFrom inputType filePath
            let results = handleString content counterTypes
            let output = getOutputString results filePath
            putStrLn output
        Right error -> fail error