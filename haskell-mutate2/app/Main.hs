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
    putStrLn $ "Input directory: " ++ show absInputDir
    putStrLn $ "Output directory: " ++ show absOutputDir

    exists <- doesDirectoryExist absInputDir

    if exists
        then do
            filePaths <- getAbsoluteDirContents inputDir
            mutants <- mapM mutateFile filePaths
            
            -- Here, mutants is a list of lists of Modules.
            -- Each list of Modules represents all the mutation variations 
            -- for that one Module.

            writeMutants outputDir mutants

            {- FOR DEBUGGING -}
            putStrLn $ "File paths: " ++ show filePaths
            putStrLn $ "Number of mutants: " ++ show (length (concat mutants))

            putStrLn $ prettyPrint $ head $ head mutants

        else 
            putStrLn $ "ERROR: Directory '" ++ inputDir ++ "' does not exist."

-- TODO
writeMutants :: FilePath -> [[Module SrcSpanInfo]] -> IO ()
writeMutants outputDir mutants = do
    let generated = ultraCombine mutants
    
    {- FOR DEBUGGING -}
    mapM pPrintNoColor (map (map (map prettyPrint)) generated)

    return ()

-- | TODO: Do this in a better way
ultraCombine :: [[Module SrcSpanInfo]] -> [[[Module SrcSpanInfo]]]
ultraCombine [] = []
ultraCombine mutants = createVariants [] mutants

createVariants :: [[Module SrcSpanInfo]]
               -> [[Module SrcSpanInfo]]
               -> [[[Module SrcSpanInfo]]]
createVariants _ [] = []
createVariants xs (y:ys) = [ x : originals | x <- y ] : createVariants (xs ++ [y]) ys
    where originals = getOriginals xs ++ getOriginals ys

getOriginals :: [[Module SrcSpanInfo]] -> [Module SrcSpanInfo]
getOriginals [] = []
getOriginals (xs:xss) | null xs   = getOriginals xss
                      | otherwise = head xs : getOriginals xss

{-
-- | Handles outputting the mutated modules for one particular source module.
writeMutants' :: FilePath -> Int -> [[Module SrcSpanInfo]] -> IO ()
writeMutants' subDir mutIndex mutants = do
    -- beforeMuts and afterMuts are of form [[Module SrcSpanInfo]]
    let beforeMuts = take mutIndex mutants
    let afterMuts  = drop (mutIndex + 1) mutants

    -- While targetMuts is of form [Module SrcSpanInfo]
    let targetMuts = mutants !! mutIndex
    
    let originals = getOriginals (beforeMuts ++ afterMuts)
    
    let combinations = [ x : originals | x <- targetMuts ]

    outputMutants subDir combinations

        where getOriginals = map head

-- | Outputs the mutant variations to folders.
outputMutants :: FilePath -> [[Module SrcSpanInfo]] -> IO ()
outputMutants _ [] = return ()
outputMutants dir mutants = do
    createDirectory $ dir </> show (length mutants)

    mapM (withCurrentDirectory (action))

    return ()
-}

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
