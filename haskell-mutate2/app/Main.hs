module Main where

import Control.Exception
import Control.Monad
import Data.Maybe (fromJust)
import Language.Haskell.Exts
import System.Environment
import System.Exit
import System.Directory
import System.FilePath ((</>), (-<.>), takeFileName, takeExtension)
import System.Process
import System.IO
import System.IO.Error
import Text.Pretty.Simple

import Mutate
import Path
import FileOp

data TestSummary = TestSummary
    { successful :: Int -- ^ Number of tests that were successfully run and passed.
    , failed     :: Int -- ^ Number of tests that ran but failed.
    , errors     :: Int -- ^ Number of errors, e.g. compilation errors.
    } deriving (Show)

-- | Data type representing how a mutant fared during a test suite, along with
--   the output of stdout or stderr, depending on the result.
data TestResult = Survived String   -- ^ Mutant survived the tests, stdout
                | Killed String     -- ^ Tests killed the mutant, stdout
                | Error String      -- ^ Tests threw an exception, stderr

incSucc (TestSummary s f e) = TestSummary (s + 1) f e
incFail (TestSummary s f e) = TestSummary s (f + 1) e
incErr  (TestSummary s f e) = TestSummary s f (e + 1)


outputSuffix = "mutants-out"
backupSuffix = "backups"


main :: IO ()
main = do
    args <- getArgs

    case args of
        "--help"       : _  -> showUsage
        "--project-dir": xs -> launchAtProject $ head xs

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
    putStrLn "--project-dir DIRECTORY   Specifies directory of cabal project."
    putStrLn "                          Should contain a .cabal file and "
    putStrLn "                          defaults to ./ if not specified"



launchAtProject :: FilePath -- ^ Path to project directory
                -> IO ()
launchAtProject projectPath = do
    -- Clean old output folder
    wipeDirIfExists outputDir

    -- Get absolute file paths to all files in the source folder
    srcFiles <- filePathsFromDir =<< srcDirFromProject projectPath

    -- Back up original file to backup directory, creating the backup
    -- directory if not already existing.
    backupOriginal filePath backupDir

    -- Create mutated ASTs of target file
    mutantModules <- mutateFile filePath

    -- Run tests with mutants
    testSummary <- runTestsWithMutants filePath mutantModules projectPath (TestSummary 0 0 0)

    -- Restore original file from backup
    restoreOriginal backupDir filePath

    -- Print summary of test runs
    printResults testSummary
    
    where backupDir = projectPath </> backupSuffix
          outputDir = projectPath </> outputSuffix





-- Functions for backing up original files and restoring them from backup.
backupOriginal :: FilePath -> FilePath -> IO ()
backupOriginal originalFile backupDir = do
    let backupFilePath = backupDir </> takeFileName originalFile
    createDirectoryIfMissing False backupDir
    putStrLn "Backing up original source file..."
    putStrLn $ "From: " ++ originalFile
    putStrLn $ "To:   " ++ backupFilePath
    withCurrentDirectory backupDir $ copyFile originalFile backupFilePath
    putStrLn $ "Backup created successfully."
    putStrLn ""

restoreOriginal :: FilePath -> FilePath -> IO ()
restoreOriginal backupDir originalFile = do
    let backupFile = backupDir </> takeFileName originalFile
    putStrLn "Restoring original file..."
    putStrLn $ "From: " ++ show backupFile
    putStrLn $ "To:   " ++ show originalFile
    copyFile backupFile originalFile
    putStrLn $ "Successfully restored original file."
    putStrLn ""


printResults :: TestSummary -> IO ()
printResults (TestSummary s f e) = do
    putStrLn ":: SUMMARY ::"
    putStrLn $ "In total, " ++ show (s + f + e) ++ " mutants were created."
    putStrLn $ "Number of mutants killed: " ++ show f
    putStrLn $ "Number of mutants that survived: " ++ show s
    putStrLn $ "Number of errors: " ++ show e


runTestsWithMutants :: FilePath             -- ^ Path to source file to mutate.
                    -> [Module SrcSpanInfo] -- ^ List of mutant ASTs.
                    -> FilePath             -- ^ Path to project directory.
                    -> TestSummary
                    -> IO TestSummary       -- ^ Result summary of test runs.
runTestsWithMutants _ [] _ testSum = return testSum
runTestsWithMutants filePath (m:ms) projectPath testSum = do
    -- Remove old file and write mutant to file.
    insertMutant

    -- Run tests with a process, returning its results
    putStrLn $ "Testing mutant... (" ++ show (length $ m:ms) ++ " left)" 
    setCurrentDirectory projectPath
    exitStatus <- readProcessWithExitCode "cabal" ["test"] ""
    let testResult = getTestResult exitStatus
    
    -- Perform actions depending on the results
    testResHandler testResult m filePath (projectPath </> outputSuffix)

    -- Update the summary values depending on the results
    let newSum = updateSummary testSum testResult

    runTestsWithMutants filePath ms projectPath newSum

    where insertMutant = do removeFile filePath
                            writeFile filePath (prettyPrint m)


getTestResult :: (ExitCode, String, String) -> TestResult
getTestResult (ExitSuccess, stdout, _) = Survived stdout
getTestResult (ExitFailure exitCode, stdout, stderr)
    | null stderr = Killed stdout
    | otherwise = Error stderr


updateSummary :: TestSummary -> TestResult -> TestSummary
updateSummary summary res = case res of
    Survived _ -> incSucc summary
    Killed _   -> incFail summary
    Error _    -> incErr summary


-- | Performs actions depending on the given TestResult:
--   If mutant was killed, tell the user but do nothing else.
--   If mutant survivied, print information and copy mutant to the given
--   directory
--   If testing returned an error, prints the information put does nohting else.
testResHandler :: TestResult            -- ^ The test result to judge.
               -> Module SrcSpanInfo    -- ^ The mutant that was tested.
               -> FilePath              -- ^ The file path of the mutant.
               -> FilePath              -- ^ The path of the output root directory.
               -> IO ()
testResHandler res mutant mutantPath outDir = case res of
    Killed stdout -> putStrLn "Testing failed; mutant was KILLED."
    
    Error stderr -> do 
        putStrLn "Testing returned an ERROR:\n"
        putStrLn stderr
    
    Survived stdout -> do
        -- Set up file names and paths
        let mutantName = takeFileName mutantPath
        let mutantsDir = outDir </> mutantName
        createDirectoryIfMissing True mutantsDir
        mutantIndex <- fmap length $ getAbsoluteDirContents mutantsDir
        let newMutantPath = mutantsDir 
                        </> mutantName 
                       -<.> show mutantIndex ++ ".hs"

        putStrLn "Testing succeeded; mutant SURVIVED.\n"
        putStrLn   "==> The mutant that was being tested was:"
        putStrLn $ "==> " ++ mutantPath

        -- Write surviving mutant to file
        putStrLn "Saving copy of mutant..."
        writeFile newMutantPath $ prettyPrint mutant
        putStrLn $ "Copy of mutant saved to: " ++ newMutantPath

        -- For debugging, mostly:
        putStrLn "==> The mutant looked like:"
        putStrLn $ prettyPrint mutant
        putStrLn ""


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
--   mutation results if successful. Otherwise prints the error message.
mutateFile :: FilePath -> IO [Module SrcSpanInfo]
mutateFile [] = return []
mutateFile path = do
    parseRes <- parseFile path

    case parseRes of
        ParseOk syntaxTree -> return $ safeMutate syntaxTree
        ParseFailed l errMsg -> showParsingError

            where showParsingError = do
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


