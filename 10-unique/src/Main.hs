module Main where

import Control.Monad.State
import System.Environment (getArgs)
import Data.List (isPrefixOf)

data UniqState = UniqState { getWord :: String, getCount :: Int, getIsLastLine :: Bool}
type PrintFunction = UniqState -> UniqState -> IO ()
type LineCreator = UniqState -> String
type LineReducer = UniqState -> String -> IO UniqState

getLineReducer :: PrintFunction -> LineReducer
getLineReducer printFun previous str = do
    let current = if getWord previous == str 
        then previous { getCount = getCount previous }
        else UniqState { getWord = str, 
                         getCount = 1, 
                         getIsLastLine = False }
    _ <- printFun previous current
    return current

handleLines :: PrintFunction -> [String] -> IO ()
handleLines printFun ls = do
    let emptyState = UniqState { getWord = "", 
                                 getCount = 0, 
                                 getIsLastLine = False }
    let f = getLineReducer printFun
    lastState <- foldM f emptyState ls
    _ <- printFun lastState UniqState { getWord = "", 
                                        getCount = 0, 
                                        getIsLastLine = True }
    return ()

takeIfCount :: Int -> UniqState -> Maybe UniqState
takeIfCount requiredValue uniqState = 
    if getCount uniqState == requiredValue 
    then Just uniqState
    else Nothing

defaultMode :: UniqState -> UniqState -> Maybe UniqState
defaultMode _ = takeIfCount 1
onlyUniq :: UniqState -> UniqState -> Maybe UniqState
onlyUniq previous current = do
    if getWord previous == getWord current
    then Nothing
    else takeIfCount 1 previous
onlyRepeated :: UniqState -> UniqState -> Maybe UniqState
onlyRepeated _ = takeIfCount 2

getLineWithCount :: UniqState -> String
getLineWithCount uniqState = show(getCount uniqState) ++ " " ++ getWord uniqState
getLineWithoutCount :: UniqState -> String
getLineWithoutCount = show . getWord

help :: String 
help = "NAME                                                                    \n\
        \   uniq-like – report or filter out repeated lines in a file           \n\
        \SYNOPSIS                                                               \n\
        \   uniq-like [ -c | -d | -u | -h ] [ - | input_file ] [output_file]    \n\
        \DESCRIPTION                                                            \n\
        \   The uniq utility reads the specified input_file comparing adjacent lines, and writes a copy of each unique input line to the output_file.  If input_file is a single dash (‘-’) or absent, the standard input is read.  If output_file is absent, standard output is used for output.  The second and succeeding copies of identical adjacent input lines are not written.  Repeated lines in the input will not be detected if they are not adjacent, so it may be necessary to sort the files first.\n\
        \                                                                       \n\
        \   The following options are available:                                \n\
        \   -c, --count                                                         \n\
        \       Precede each output line with the count of the number of times the line occurred in the input, followed by a single space. \n\
        \   -d, --repeated                                                      \n\
        \       Output a single copy of each line that is repeated in the input.\n\
        \   -u, --unique                                                        \n\
        \       Only output lines that are not repeated in the input.           \n\
        \   -h, --help                                                          \n\
        \       Print help                                                      \n\
        \"      

isConsoleInput :: [String] -> Bool
isConsoleInput args = "-" `elem` args 

getFileName :: [String] -> Maybe FilePath
getFileName args = f $ dropWhile ( isPrefixOf "-" ) args where 
    f (inputFile:_) = Just inputFile
    f _ = Nothing

getInputContent :: [String] -> IO String 
getInputContent args = 
    if isConsoleInput args
    then getContents
    else case getFileName args of 
        Just filePath -> readFile filePath 
        _ -> fail $ "can't find file name among input parameters: " ++ concat args

getPrintFunction :: [String] -> Maybe (Bool -> String -> IO())
getPrintFunction args = f $ dropWhile ( isPrefixOf "-" ) args where
    consoleFun isLastLine = if isLastLine then putStr else putStrLn
    fileFun file isLastLine =  writeFile file

    f [] = Just consoleFun
    f (x: _) | isConsoleInput args  = Just $ fileFun x
    f (_: []) = Just consoleFun
    f (_: xs) = fileFun <$> getFileName xs


main :: IO ()
main = do 
    args <- getArgs 
    if null args || "-h" `elem` args 
    then putStrLn help
    else do 
        inputContent <- getInputContent args
        let filterFunction | "-d" `elem` args = onlyRepeated
                           | "-u" `elem` args = onlyUniq
                           | otherwise = defaultMode
        let toString = if "-c" `elem` args 
            then getLineWithCount 
            else getLineWithoutCount 
        let composedFunction p c = toString <$> filterFunction p c

        printFunction <- case getPrintFunction args of 
                Just f -> return (\p c -> forM_ (composedFunction p c) (f $ getIsLastLine c)) 
                _ -> fail $ "can't detect output type: " ++ concat args
        
        handleLines printFunction (lines inputContent) 