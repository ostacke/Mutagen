module Main where

import System.Environment
import System.Directory
import System.FilePath ((</>))
import System.Process

import Text.Pretty.Simple

import Language.Haskell.Exts
import Control.Monad
import Mutate

main :: IO ()
main = do
    args <- getArgs

    case args of
        "--help"       : _  -> showUsage
        "--input-dir"  : xs -> launchAtDir (head xs) (getOutputDir $ tail xs)

        _ -> showUsage

getOutputDir :: [String] -> String
getOutputDir []     = "./out"
getOutputDir (x:xs) = case x of
    "--output-dir" -> head xs
    _ -> getOutputDir xs

showUsage = do 
    putStrLn "Usage: haskell-mutate2-exe [OPTION]..."
    putStrLn "--help                    Shows this text."
    putStrLn "--input-dir DIRECTORY     Specifies location of files to mutate."
    putStrLn "--output-dir DIRECTORY    Specifies output location of result."
    putStrLn "                          Defaults to ./out."

launchAtDir :: FilePath -> FilePath -> IO ()
launchAtDir inputDir outputDir = do
    absInputDir <- canonicalizePath inputDir
    absOutputDir <- canonicalizePath outputDir

    {- FOR DEBUGGING -}
    print absInputDir
    print absOutputDir

    exists <- doesDirectoryExist absInputDir

    if exists
        then do
            filePaths <- getAbsoluteDirContents inputDir
            mutants <- mutateFiles filePaths
            --writeMutantsToFiles

            {- FOR DEBUGGING -}
            putStrLn $ show filePaths
            putStrLn $ "Number of mutants: "  ++ show (length mutants)


        else 
            putStrLn $ "ERROR: Directory '" ++ inputDir ++ "' does not exist."


-- | Given a path to a directory, returns a list of the absolute paths for 
--   every file at the directory.
getAbsoluteDirContents :: FilePath -> IO [FilePath]
getAbsoluteDirContents dir = do
    contents <- listDirectory dir
    let relativePaths = map (dir </>) contents
    mapM canonicalizePath relativePaths

-- | Runs mutate on all files from a given list of file paths, returns 
--   a list of mutated Modules
mutateFiles :: [FilePath] -> IO ([[Module SrcSpanInfo]])
mutateFiles paths = mapM mutateFile paths

mutateFile :: FilePath -> IO ([Module SrcSpanInfo])
mutateFile [] = return []
mutateFile path = do
    parseRes <- parseFile path

    case parseRes of
        ParseOk ast -> do
            let mutantTrees = mutate ast
            return mutantTrees
        ParseFailed l errMsg -> do
            putStrLn $ "Parsing failed:"
            putStrLn $ ""
            putStrLn $ errMsg
            return []

{- 
main :: IO ()
main = do
    args <- getArgs

    case args of
        "--dir"     : xs    -> launchAtDir $ head xs
        "--help"    : xs    -> showUsage
        "--version" : xs    -> putStrLn version
        _                   -> launch args

version :: String




launchAtDir :: String -> IO ()
launchAtDir dir = do
    exists <- doesDirectoryExist dir

    if not exists
        then putStrLn $ "ERROR: Directory '" ++ dir ++ "' does not exist."
        else do
            files <- getAbsoluteDirContents dir
            mutateOnPaths dir $ filter (\xs -> drop (length xs - 3) xs == ".hs") files



launch :: [String] -> IO ()
launch args = case length args of
    0 -> showUsage
    1 -> do
        putStrLn $ ""
        putStrLn $ "Please specify a path with the --dir option."
        putStrLn $ ""

    _ -> showUsage

mutateOnPaths :: FilePath -> [String] -> IO ()
mutateOnPaths _ [] = return ()
mutateOnPaths outputDir (x:xs) = do
    res <- parseFile x
    case res of
        ParseOk ast -> do
            putStrLn $ "Parsing successful, creating mutants..."
            let mutantTrees = mutate ast

            -- FOR DEBUGGING
            -- pPrintNoColor mutantTrees

            putStrLn $ "Mutants created, writing to output files..."
            writeMutants outputDir x mutantTrees

            putStrLn $ "Finished writing mutants to files."

        ParseFailed l errMsg -> do
            putStrLn $ "Parsing failed:"
            putStrLn $ ""
            putStrLn $ errMsg

    mutateOnPaths outputDir xs


writeMutants :: FilePath -> String -> [Module l] -> IO ()
writeMutants _ _ []        = return ()
writeMutants outputDirParent path (x:xs) = do
    let outputDir = outputDirParent </> "out" </> show (length xs)
    createDirectoryIfMissing True outputDir

    let mutantPath = path ++ "-mutant-" ++ show (length xs) ++ ".hs"

    {- FOR DEBUGGING -}

    putStrLn $ ""
    putStrLn $ ":: Path of mutant: " ++ mutantPath
    putStrLn $ ":: Prettyprint of mutant: "
    putStrLn $ ""
    putStrLn $ prettyPrint x

    withCurrentDirectory outputDir $ writeFile path (prettyPrint x)

    writeMutants outputDirParent path xs

-}
