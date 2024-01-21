module Main where

import System.Environment (getArgs)
import Data.List (isPrefixOf)
import Text.Printf (printf)

type PrintFunc = Bool -> String -> Int -> IO()
type FilterFunc = Int -> Bool

handleLines :: [String] -> FilterFunc -> PrintFunc -> IO ()
handleLines [] _ _ = return ()
handleLines allLines@(currentLine: _) filterFun printFun = do
    let (prefix, suffix) = span (== currentLine) allLines
    let isLastLine = null suffix
    let count = length prefix

    if filterFun count
    then printFun isLastLine currentLine count
    else return ()

    handleLines suffix filterFun printFun 

help :: String 
help = "NAME                                                                    \n\
        \   uniq-like – report or filter out repeated lines in a file           \n\
        \SYNOPSIS                                                               \n\
        \   uniq-like [ -c | -d | -u | -h ] [ - | input_file ] [output_file]    \n\
        \DESCRIPTION                                                            \n\
        \   The uniq utility reads the specified input_file comparing adjacent lines, and writes a copy of each unique input line to the output_file. If input_file is a single dash (‘-’) or absent, the standard input is read. If output_file is absent, standard output is used for output. The second and succeeding copies of identical adjacent input lines are not written. Repeated lines in the input will not be detected if they are not adjacent, so it may be necessary to sort the files first.\n\
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
        _ -> fail $ "can't find file name among input parameters: " ++ (concat args)

getPrintFunction :: [String] -> Maybe (Bool -> String -> IO ())
getPrintFunction args = f $ dropWhile ( isPrefixOf "-" ) args where
    consoleFunc isLastLine = if isLastLine then putStr else putStrLn
    fileFunc file isLastLine = \s -> appendFile file $ if isLastLine then s else s ++ "\n"

    f [] = Just $ consoleFunc
    f (x: _) | isConsoleInput args  = Just $ fileFunc x
    f (_: []) = Just $ consoleFunc
    f (_: xs) = fileFunc <$> getFileName xs

main :: IO ()
main = do 
    args <- getArgs 
    if null args || "-h" `elem` args || "--help" `elem` args 
    then putStrLn help
    else do 
        inputContent <- getInputContent args
        let filterFunction | "-d" `elem` args || "--repeated" `elem` args = (> 1)
                           | "-u" `elem` args || "--unique" `elem` args = (== 1)
                           | otherwise = (>= 1)
        let toString str count = if "-c" `elem` args 
            then (printf "%4d" count) ++ " " ++ str  
            else str 

        printFunction <- case getPrintFunction args of 
                Just f -> return (\isLastLine currentLine count -> f isLastLine $ toString currentLine count)
                _ -> fail $ "can't detect output type: " ++ (concat args)
        
        handleLines (lines inputContent) filterFunction printFunction