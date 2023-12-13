module Main where

import qualified Data.Text.Encoding as TSE
import qualified Data.ByteString as B
import Data.Text (singleton)
import Data.Char (isSpace)

data CounterType = BytesCounterType | SymbolsCounterType | WordsCounterType | LinesCounterType deriving (Show, Eq, Enum) 
data Counter = SimpleCounter { getType::CounterType, getResult::Int } | 
               WordsCounter { getType::CounterType, getResult::Int, getIntemediateResult::Int, isWord::Bool }

handleSymbol :: Char -> [Counter] -> [Counter]
handleSymbol symbol counters = map f counters where 
    f counter = case (getType counter) of 
        SymbolsCounterType -> counter { getResult = getResult counter + 1 }
        BytesCounterType -> counter { getResult = getResult counter + increment } where
            text = singleton symbol
            byteString = TSE.encodeUtf8 text
            increment = B.length byteString 
        WordsCounterType -> updatedCounter { getResult = getIntemediateResult updatedCounter + if isWord updatedCounter then 1 else 0 } where
            isSpaceSymbol = isSpace symbol
            increment = if isSpaceSymbol && isWord counter then 1 else 0
            isWordValue = not isSpaceSymbol
            updatedCounter = counter { isWord = isWordValue, getIntemediateResult = getIntemediateResult counter + increment}
        LinesCounterType -> counter { getResult = getResult counter + increment } where
            increment = if symbol == '\n' then 1 else 0

createCounters :: [CounterType] -> [Counter]
createCounters counterTypes = map f counterTypes where 
    f t = case t of 
        WordsCounterType -> WordsCounter { getType = t, getResult = 0, isWord = False, getIntemediateResult = 0 }
        otherwise ->  SimpleCounter { getType = t, getResult = 0 }

handleString :: [Char] -> [CounterType] -> [(CounterType, Int)]
handleString chars counterTypes = let
    counters = createCounters counterTypes
    handledCounters = foldr handleSymbol counters chars 
    results = map (\c -> (getType c, getResult c)) handledCounters
    in results

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