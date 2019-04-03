module Main where

import Data.Maybe (fromJust)
import Language.Haskell.Exts
import System.Environment
import System.Directory
import System.FilePath ((</>), (-<.>), takeFileName, takeExtension)
import System.Process

import FileOp
import Results
import Mutate
import Path

outputSuffix = "mutants-out"
backupSuffix = "backups"
emptyRes = ResultSummary 0 0 0


main :: IO ()
main = do
    args <- getArgs

    case args of
        "--help"       : _  -> showUsage
        "--project-dir": xs -> makeAbsolute (head xs) >>= launchAtProject
        _ -> showUsage


showUsage :: IO ()
showUsage = do 
    putStrLn "Usage: haskell-mutate2-exe [OPTION]..."
    putStrLn "--help                    Shows this text."
    putStrLn "--project-dir DIRECTORY   Specifies directory of cabal project."
    putStrLn "                          Should contain a .cabal file and "
    putStrLn "                          defaults to ./ if not specified"


launchAtProject :: FilePath -> IO ()
launchAtProject projectPath = do
    -- Check if there already exists a "MutateInject.hs", since we do not 
    -- want to overwrite the existing file in the off-chance that it isn't
    -- our injected file.
    checkInjectExists

    -- Build and run the unmodified test suites to see that they work when 
    -- unmodified.
    checkTestSuites projectPath

    -- Clean old output folder
    wipeDirIfExists outputDir
    
    -- Get absolute file paths to all files in the source folder, these are 
    -- the files that will be mutated.
    -- NOTE: This needs to run BEFORE injecting MutateInject.hs
    srcFiles <- filePathsFromDir =<< srcDirFromProject projectPath

    -- Copy MutateInject.hs to project directory, so that the mutated source 
    -- files have access to the required module
    -- NOTE: This needs to happen AFTER getting the source file paths, 
    --       otherwise the program tries to mutate our injected file.
    srcDirFromProject projectPath >>= copyMutateInject

    -- Add "MutateInject" to "other-modules" of the project .cabal file, 
    -- after backing up.
    cabalPathFromProject projectPath >>= \x -> backupOriginal x backupDir
    cabalPathFromProject projectPath >>= \x -> cabalAddModule "MutateInject" x

    -- Mutate and test all source files in turn, generating a 
    -- result summary, then sums the results. Also saves surviving 
    -- mutants to file.
    rs <- mapM (flip runRoutine projectPath) srcFiles
    let resultSummary = foldl (|+|) emptyRes rs

    -- Remove the injected MutateInject.hs file and restore the original 
    -- .cabal file.
    srcDirFromProject projectPath >>= cleanMutateInject
    cabalPathFromProject projectPath >>= \x -> restoreOriginal backupDir x

    printResults resultSummary
    
    where backupDir = projectPath </> backupSuffix
          outputDir = projectPath </> outputSuffix
          checkInjectExists = do
              putStrLn "==> Looking for existing MutateInject.hs..."
              srcDir <- srcDirFromProject projectPath
              exists <- doesFileExist $ srcDir </> "MutateInject.hs"
              if exists then Prelude.error "MutateInject.hs already exists in \
                                           \source directory! Aborting."
                        else putStrLn "None found, proceeding."


checkTestSuites :: FilePath -> IO ()
checkTestSuites projectPath = do
    putStrLn "==> Running tests on ORIGINAL code..."

    testSuites <- testSuitesFromProject projectPath
    setCurrentDirectory projectPath
    results <- runTestSuites testSuites emptyRes

    case results of
        ResultSummary _ 0 0 -> putStrLn "No problems found."
        ResultSummary _ k e -> do
            putStrLn "==> WARNING:"
            putStrLn $ "There were " ++ show k ++ " failed test(s) and "
            putStrLn $ show e ++ " error(s) when testing the ORIGINAL code."
            putStrLn ""


runTestSuites :: [String] -> ResultSummary -> IO ResultSummary
runTestSuites [] s = return s
runTestSuites (x:xs) s = do
    exitCode <- readProcessWithExitCode "cabal" ["test", x] ""
    let res = getTestResult exitCode
    let summary = updateSummary s res
    case res of
        Killed stdout -> do
            putStrLn $ "WARNING: Test suite failed:\n"
            putStrLn stdout
            runTestSuites xs summary
        Error stderr -> do
            putStrLn $ "WARNING: There was an error when running testsuite:\n"
            putStrLn stderr
            runTestSuites xs summary
        _ -> runTestSuites xs summary


-- | Given a path to a file, project, and a TestSummary, runs the process of 
--   backing up the original file, mutating it, testing it, and restoring the 
--   original file back. Then returns the updated TestSummary.
runRoutine :: FilePath -> FilePath -> IO ResultSummary
runRoutine filePath projectPath = do
    putStrLn $ "Performing tests on mutant: " ++ takeFileName filePath

    -- Back up the original file to the backup directory, creating the backup
    -- directory if not already existing.
    backupOriginal filePath backupDir

    -- Create mutated ASTs of the target file.
    mutantModules <- mutateFile filePath

    -- Run the tests with the mutant, return the results from the test
    testSummary <- runTestsWithMutants filePath mutantModules projectPath emptyRes

    -- Restore the original file from the backup location
    restoreOriginal backupDir filePath

    return testSummary

    where backupDir = projectPath </> backupSuffix
          outputDir = projectPath </> outputSuffix


runTestsWithMutants :: FilePath             -- ^ Path to source file to mutate.
                    -> [Module SrcSpanInfo] -- ^ List of mutant ASTs.
                    -> FilePath             -- ^ Path to project directory.
                    -> ResultSummary
                    -> IO ResultSummary     -- ^ Result summary of test runs.
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
    handleTestResult testResult m filePath (projectPath </> outputSuffix)

    -- Update the summary values depending on the results
    let newSum = updateSummary testSum testResult

    runTestsWithMutants filePath ms projectPath newSum

    where insertMutant = do removeFile filePath
                            writeFile filePath (prettyPrint m)


-- | Performs actions depending on the given TestResult:
--   If mutant was killed, tell the user but do nothing else.
--   If mutant survivied, print information and copy mutant to the given
--   directory
--   If testing returned an error, prints the information put does nothing else.
handleTestResult :: TestResult            -- ^ The test result to judge.
                 -> Module SrcSpanInfo    -- ^ The mutant that was tested.
                 -> FilePath              -- ^ The file path of the mutant.
                 -> FilePath              -- ^ The path of the output root directory.
                 -> IO ()
handleTestResult res mutant mutantPath outDir = case res of
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


-- | Attemps to parse a module at the given file path, returning the 
--   mutation results if successful. Otherwise prints the error message.
mutateFile :: FilePath -> IO [Module SrcSpanInfo]
mutateFile [] = return []
mutateFile path = do
    parseRes <- parseFile path

    case parseRes of
        ParseOk syntaxTree -> 
            return $ map injectMutateInject $ safeMutate syntaxTree

        ParseFailed l errMsg -> 
            showParsingError

            where showParsingError = do
                    putStrLn "PARSING FAILED:"
                    putStrLn ""
                    putStrLn errMsg
                    putStrLn ""
                    return []

