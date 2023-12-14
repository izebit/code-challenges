{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import qualified Data.Text.Encoding as TSE
import qualified Data.ByteString as B
import Data.Text (singleton)
import Data.Char (isSpace)

data CounterType = BytesCounterType | SymbolsCounterType | WordsCounterType | LinesCounterType deriving (Show, Eq, Enum) 
data Counter = SimpleCounter { getType::CounterType, getResult::Int } | 
               WordsCounter { getType::CounterType, getResult::Int, getIntemediateResult::Int, isWord::Bool }

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


data InputType = Console | File deriving (Enum, Eq, Show)
data InputParamereter = InputParamereter { inputType:: InputType, filePath::Maybe String, counterTypes::[CounterType]}
getInputParameter :: String -> Either InputParamereter String 
getInputParameter args = do

    Right "error!"

getOutputString :: [(CounterType, Int)] -> String
getOutputString results = let 
    resultTokens = map (\pair -> (show $ fst pair) ++ " " ++ (show $ snd pair)) results
    output = foldr (\t s -> if s == "" then t else t ++ ", " ++ s) "" resultTokens
    in output



main :: IO ()
main = do 
    content <- readFile "test.txt"
    let results = handleString content [BytesCounterType, SymbolsCounterType, WordsCounterType, LinesCounterType]
    putStrLn $ getOutputString results