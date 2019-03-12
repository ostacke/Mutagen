module Main where

import System.Environment
import System.Directory
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
    putStrLn $ "Attempting to run haskell-mutate2 at " ++ path
    putStrLn $ ""

    let cdDir    = "cd " ++ path ++ ";"
    let announce = "echo MOVED TO THIS DIRECTORY: ;"
    let pwd      = "pwd ;"

    r <- createProcess (shell $ cdDir ++ announce ++ pwd)

    putStrLn $ "Finished."

launch :: [String] -> IO ()
launch args = case length args of
    0 -> showUsage
    1 -> do
        putStrLn $ ""
        putStrLn $ "haskell-mutate2 version 0.1.0.0"
        putStrLn $ "Attempting to parse and mutate file..."
        putStrLn $ ""

        mutateOnPath (head args)

    _ -> showUsage

mutateOnPath :: String -> IO ()
mutateOnPath path = do
    res <- parseFile path
    case res of
        ParseOk ast -> do
            putStrLn $ "Parsing successful, creating mutants..."
            let mutantTrees = mutate ast

            -- FOR DEBUGGING
            -- pPrintNoColor mutantTrees

            putStrLn $ "Mutants created, writing to output files..."
            writeMutants path mutantTrees

            putStrLn $ "Finished writing mutants to files."

        ParseFailed l errMsg -> do
            putStrLn $ "Parsing failed:"
            putStrLn $ ""
            putStrLn $ errMsg

writeMutants :: String -> [Module l] -> IO ()
writeMutants _ []        = return ()
writeMutants path (x:xs) = do
    let outputDir = "out"
    createDirectoryIfMissing True outputDir


    let mutantPath = path ++ "-mutant-" ++ show (length xs) ++ ".hs"

    {- FOR DEBUGGING -}

    putStrLn $ ""
    putStrLn $ ":: Path of mutant: " ++ mutantPath
    putStrLn $ ":: Prettyprint of mutant: "
    putStrLn $ ""
    putStrLn $ prettyPrint x

    -- withCurrentDirectory outputDir $ writeFile mutantPath (prettyPrint x)

    writeMutants path xs
