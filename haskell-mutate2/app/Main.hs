module Main where

import System.Environment
import System.Directory
import System.FilePath ((</>))
import System.Process

import Text.Pretty.Simple

import Language.Haskell.Exts

import Mutate

main :: IO ()
main = do
    args <- getArgs

    case args of
        "--dir"     : xs    -> launchAtDir $ head xs
        "--help"    : xs    -> showUsage
        "--version" : xs    -> putStrLn version
        _                   -> launch args

version :: String
version = "haskell-mutate2 version 0.1.0.0"

showUsage :: IO ()
showUsage = do
    putStrLn "haskell-mutate2 version 0.1.0.0"
    putStrLn "Usage: haskell-mutate2 SOURCE"

launchAtDir :: String -> IO ()
launchAtDir path = do
    exists <- doesDirectoryExist path

    if not exists
        then putStrLn $ "ERROR: Directory '" ++ path ++ "' does not exist."
        else do
            files <- getAbsoluteDirContents path
            mutateOnPaths $ filter (\xs -> drop (length xs - 3) xs == ".hs") files

getAbsoluteDirContents :: String -> IO [FilePath]
getAbsoluteDirContents dir = do
    contents <- listDirectory dir
    return $ map (dir </>) contents

launch :: [String] -> IO ()
launch args = case length args of
    0 -> showUsage
    1 -> do
        putStrLn $ ""
        putStrLn $ "haskell-mutate2 version 0.1.0.0"
        putStrLn $ "Attempting to parse and mutate file..."
        putStrLn $ ""

        mutateOnPaths [(head args)]

    _ -> showUsage

mutateOnPaths :: [String] -> IO ()
mutateOnPaths []     = return ()
mutateOnPaths (x:xs) = do
    res <- parseFile x
    case res of
        ParseOk ast -> do
            putStrLn $ "Parsing successful, creating mutants..."
            let mutantTrees = mutate ast

            -- FOR DEBUGGING
            -- pPrintNoColor mutantTrees

            putStrLn $ "Mutants created, writing to output files..."
            writeMutants x mutantTrees

            putStrLn $ "Finished writing mutants to files."

        ParseFailed l errMsg -> do
            putStrLn $ "Parsing failed:"
            putStrLn $ ""
            putStrLn $ errMsg

    mutateOnPaths xs


writeMutants :: String -> [Module l] -> IO ()
writeMutants _ []        = return ()
writeMutants path (x:xs) = do
    let outputDir = "out/out" ++ show (length xs)
    createDirectoryIfMissing True outputDir

    let mutantPath = path ++ "-mutant-" ++ show (length xs) ++ ".hs"

    {- FOR DEBUGGING -}

    putStrLn $ ""
    putStrLn $ ":: Path of mutant: " ++ mutantPath
    putStrLn $ ":: Prettyprint of mutant: "
    putStrLn $ ""
    putStrLn $ prettyPrint x

    withCurrentDirectory outputDir $ writeFile path (prettyPrint x)

    writeMutants path xs


