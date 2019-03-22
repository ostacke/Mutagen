module Main where

import Control.Monad
import Data.Maybe (fromJust)
import Language.Haskell.Exts
import System.Environment
import System.Directory
import System.FilePath ((</>), takeFileName, takeExtension)
import System.Process
import System.IO
import Text.Pretty.Simple

import Mutate


main :: IO ()
main = do
    args <- getArgs

    case args of
        "--help"       : _  -> showUsage
        "--input-file" : xs -> launchAtProject (head xs) (getProjectDir $ tail xs)
        "--input-dir"  : xs -> launchAtDir (head xs) (getOutputDir $ tail xs)

        _ -> showUsage


getOutputDir :: [String] -> String
getOutputDir []     = "./out"
getOutputDir (x:xs) = case x of
    "--output-dir" -> head xs
    _ -> getOutputDir xs


getProjectDir :: [String] -> String
getProjectDir [] = "./"
getProjectDir (x:xs) = case x of
    "--project-dir" -> head xs
    _ -> getProjectDir xs


showUsage :: IO ()
showUsage = do 
    putStrLn "Usage: haskell-mutate2-exe [OPTION]..."
    putStrLn "--help                    Shows this text."
    putStrLn "--input-file FILE         Specifies input file to mutate."
    putStrLn "--input-dir DIRECTORY     Specifies location of files to mutate."
    putStrLn "--output-dir DIRECTORY    Specifies output location of result."
    putStrLn "                          Defaults to ./out."
    putStrLn "--project-dir DIRECTORY   Specifies directory of cabal project."
    putStrLn "                          Should contain a .cabal file and "
    putStrLn "                          defaults to ./"



launchAtProject :: FilePath -> FilePath -> IO ()
launchAtProject fPath pPath = do
    projectPath <- makeAbsolute pPath
    filePath <- makeAbsolute fPath
    projContents <- getAbsoluteDirContents projectPath

    -- Create folder to put backup of original source file
    let backupDir = projectPath </> "backups"
    createDirectoryIfMissing False backupDir
    
    -- Copy file to backup at new backup path
    withCurrentDirectory backupDir $ copyFile filePath (backupDir </> takeFileName filePath)

    -- Get mutations of target file
    mutantModules <- mutateFile filePath
    putStrLn $ (show $ length mutantModules) ++ " mutants created."

    -- Run tests with mutants
    runTestsWithMutants filePath mutantModules projectPath

    return ()
    

runTestsWithMutants :: FilePath -> [Module SrcSpanInfo] -> FilePath -> IO ()
runTestsWithMutants _ [] _ = putStrLn "ASDFASDFASDF"
runTestsWithMutants filePath (m:ms) projectPath = do
    -- Remove old file and write mutant to file
    removeFile filePath
    putStrLn "Writing mutant to file..."
    writeFile filePath (prettyPrint m)

    putStrLn ""
    putStrLn "Attempting to run 'cabal test' with the follwing mutant: "
    putStrLn ""
    putStrLn $ prettyPrint m
    putStrLn ""

    -- Run cabal test with the new mutant
    setCurrentDirectory projectPath
    callCommand "cabal test"

    runTestsWithMutants filePath ms projectPath


-- | Tries to run the mutation process on all files at the input directory, 
--   then outputting the result. Fails if the input directory does not exist.
launchAtDir :: FilePath -> FilePath -> IO ()
launchAtDir inputDir outputDir = do
    absInputDir <- canonicalizePath inputDir
    absOutputDir <- canonicalizePath outputDir

    exists <- doesDirectoryExist absInputDir

    if exists
        then do
            -- Performs the actual mutating and outputs to files
            filePaths <- getAbsoluteDirContents inputDir
            let hsPaths = filter (\x -> takeExtension x == ".hs") filePaths
            mutants <- mapM mutateFile hsPaths

            let combinations = combineMutants mutants
            outputMutants absOutputDir combinations

            -- Prints some information of what happened:
            putStrLn ""
            putStrLn "File paths of input files: "
            mapM_ putStrLn hsPaths
            putStrLn ""

            putStrLn $ "Input directory: " ++ show absInputDir
            putStrLn $ "Output directory: " ++ show absOutputDir
            putStrLn ""

            putStrLn $ "Total number of mutants: " ++ show (length (concat mutants))

        else 
            putStrLn $ "ERROR: Directory '" ++ inputDir ++ "' does not exist."


-- | Attemps to parse a module at the given file path, returning the 
--   mutation results if successful.
mutateFile :: FilePath -> IO [Module SrcSpanInfo]
mutateFile [] = return []
mutateFile path = do
    parseRes <- parseFile path

    case parseRes of
        ParseOk syntaxTree -> do
            let mutantTrees = safeMutate syntaxTree
            return mutantTrees

        ParseFailed l errMsg -> do
            putStrLn "PARSING FAILED:"
            putStrLn ""
            putStrLn errMsg
            putStrLn ""
            return []
    
    
-- | Invokes combineMutants' with intended first parameter.
combineMutants :: [[Module SrcSpanInfo]] -> [[[Module SrcSpanInfo]]]
combineMutants = combineMutants' []


-- | Given a list of mutations lists (from `map mutate xs`), returns a 
--   list of groups created from the combinations of the different modules, 
--   where:
--   * For each group of modules, exactly one module is mutated and contains 
--     exactly one mutation, while the rest of the modules are unchanged from 
--     their original versions.
--   * Each group of modules contains all the given modules. In other words, 
--     no group will be missing a module.
combineMutants' :: [[Module SrcSpanInfo]]
                -> [[Module SrcSpanInfo]]
                -> [[[Module SrcSpanInfo]]]
combineMutants' _ [] = []
combineMutants' xs (y:ys) = 
    [ x : originals | x <- y ] : combineMutants' (xs ++ [y]) ys
        where originals = getOriginals xs ++ getOriginals ys
    -- Creates lists from all the mutations of one module. For each mutated 
    -- module, the original versions of the other modules are also in the list. 


-- | Given a list of mutation lists (from running `map mutate xs`), returns 
--   the first element of each list. Basically a glorified `map head xs`.
getOriginals :: [[Module SrcSpanInfo]] -> [Module SrcSpanInfo]
getOriginals [] = []
getOriginals (xs:xss) | null xs   = getOriginals xss
                      | otherwise = head xs : getOriginals xss


-- | Recursively creates a tree-like folder structure at the input directory
--   and writes the mutated files.
outputMutants :: FilePath -> [[[Module SrcSpanInfo]]] -> IO ()
outputMutants _ [] = return ()
outputMutants dir (x:xs) = do
    let workingDir = dir </> show (length (x:xs))
    createDirectoryIfMissing True workingDir
    outputMutants' workingDir x

    outputMutants dir xs


-- | In the input files for this function, all the mutations are of the same 
--   file, with the other files always being unchanged from their originals.
outputMutants' :: FilePath -> [[Module SrcSpanInfo]] -> IO ()
outputMutants' _ [] = return ()
outputMutants' dir (x:xs) = do
    let workingDir = dir </> show (length (x:xs))
    createDirectoryIfMissing True workingDir
    outputMutants'' workingDir x

    outputMutants' dir xs


-- | Each list of inputs to this function represents a group of files where 
--   exactly one of the files contains exactly one mutation.
outputMutants'' :: FilePath -> [Module SrcSpanInfo] -> IO ()
outputMutants'' _ [] = return ()
outputMutants'' dir (x:xs) = do
    let newPath = dir </> (takeFileName $ fromJust (getModulePath x))

    handle <- openFile newPath WriteMode
    hPutStrLn handle (prettyPrint x)
    hClose handle
    
    outputMutants'' dir xs


-- | Returns the absolue path of a Module, as specified in the SrcSpanInfo
getModulePath :: Module SrcSpanInfo -> Maybe FilePath
getModulePath mod = case mod of
    Module (SrcSpanInfo (SrcSpan fileName _ _ _ _) _) _ _ _ _ -> Just fileName
    _ -> Nothing


-- | Given a path to a directory, returns a list of the absolute paths for 
--   every file at the directory.
getAbsoluteDirContents :: FilePath -> IO [FilePath]
getAbsoluteDirContents dir = do
    contents <- listDirectory dir
    let relativePaths = map (dir </>) contents
    mapM canonicalizePath relativePaths

