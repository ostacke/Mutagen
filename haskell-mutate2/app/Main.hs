module Main where

import System.Environment
import System.Directory
import System.FilePath ((</>), takeFileName)
import System.Process
import System.IO

import Data.Maybe (fromJust)

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

    exists <- doesDirectoryExist absInputDir

    if exists
        then do
            filePaths <- getAbsoluteDirContents inputDir
            mutants <- mapM mutateFile filePaths
            
            -- Here, mutants is a list of lists of Modules.
            -- Each list of Modules represents all the mutation variations 
            -- for that one Module.

            let combinations = combineMutants [] mutants
            outputMutants absOutputDir combinations

            putStrLn ""
            putStrLn "File paths of input files: "
            mapM_ putStrLn filePaths
            putStrLn ""

            putStrLn $ "Input directory: " ++ show absInputDir
            putStrLn $ "Output directory: " ++ show absOutputDir
            putStrLn ""

            putStrLn $ "Total number of mutants: " ++ show (length (concat mutants))

        else 
            putStrLn $ "ERROR: Directory '" ++ inputDir ++ "' does not exist."

-- TODO
writeMutants :: FilePath -> [[Module SrcSpanInfo]] -> IO ()
writeMutants outputDir mutants = do
    let combinations = combineMutants [] mutants
    
    {- FOR DEBUGGING -}
    mapM_ pPrintNoColor $ map (map (map prettyPrint)) combinations

    outputMutants outputDir combinations

combineMutants :: [[Module SrcSpanInfo]]
               -> [[Module SrcSpanInfo]]
               -> [[[Module SrcSpanInfo]]]
combineMutants _ [] = []
combineMutants xs (y:ys) = 
    [ x : originals | x <- y ] : combineMutants (xs ++ [y]) ys
        where originals = getOriginals xs ++ getOriginals ys

getOriginals :: [[Module SrcSpanInfo]] -> [Module SrcSpanInfo]
getOriginals [] = []
getOriginals (xs:xss) | null xs   = getOriginals xss
                      | otherwise = head xs : getOriginals xss

outputMutants :: FilePath -> [[[Module SrcSpanInfo]]] -> IO ()
outputMutants _ [] = return ()
outputMutants dir (x:xs) = do
    let workingDir = dir </> show (length (x:xs))
    createDirectoryIfMissing True workingDir
    outputMutants' workingDir x

    outputMutants dir xs

outputMutants' :: FilePath -> [[Module SrcSpanInfo]] -> IO ()
outputMutants' _ [] = return ()
outputMutants' dir (x:xs) = do
    let workingDir = dir </> show (length (x:xs))
    createDirectoryIfMissing True workingDir
    outputMutants'' workingDir x

    outputMutants' dir xs

outputMutants'' :: FilePath -> [Module SrcSpanInfo] -> IO ()
outputMutants'' _ [] = return ()
outputMutants'' dir (x:xs) = do
    let newPath = dir </> (takeFileName $ fromJust (getPath x))

    handle <- openFile newPath WriteMode
    hPutStrLn handle (prettyPrint x)
    hClose handle
    
    outputMutants'' dir xs

getPath :: Module SrcSpanInfo -> Maybe FilePath
getPath mod = case mod of
    Module (SrcSpanInfo (SrcSpan fileName _ _ _ _) _) _ _ _ _ -> Just fileName
    _ -> Nothing

-- | Given a path to a directory, returns a list of the absolute paths for 
--   every file at the directory.
getAbsoluteDirContents :: FilePath -> IO [FilePath]
getAbsoluteDirContents dir = do
    contents <- listDirectory dir
    let relativePaths = map (dir </>) contents
    mapM canonicalizePath relativePaths

-- | Attemps to parse a module at the given file path, returning the 
--   mutation results if successful.
mutateFile :: FilePath -> IO [Module SrcSpanInfo]
mutateFile [] = return []
mutateFile path = do
    parseRes <- parseFile path

    case parseRes of
        ParseOk ast -> do
            let mutantTrees = mutate ast
            
            {- FOR DEBUGGING -}
            -- print mutantTrees
            
            return mutantTrees
        ParseFailed l errMsg -> do
            putStrLn "PARSING FAILED:"
            putStrLn ""
            putStrLn errMsg
            putStrLn ""
            return []
