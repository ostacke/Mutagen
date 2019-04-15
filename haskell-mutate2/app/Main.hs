module Main where

import Control.Concurrent
import Control.Monad
import Data.Maybe (fromJust)
import Language.Haskell.Exts
import Path.IO (copyDirRecur)
import qualified Path.Internal as P
import System.Environment
import System.Exit
import System.Directory
import System.FilePath ((</>), (-<.>), takeFileName, takeExtension)
import System.Posix.Signals
import System.Process
import System.Timeout

import Helper.FileOp
import Helper.Path
import Helper.PreRun
import Helper.Results
import Mutate

type Timeout = Int

outputSuffix = "mutants-out"
backupSuffix = "backups"
emptyRes = ResultSummary 0 0 0

main :: IO ()
main = do
    args <- getArgs
    
    case args of
        "--help"       : _  -> showUsage
        "--project-dir": xs -> 
            makeAbsolute (head xs) >>= \x -> launchAtProject timeout x

            where timeout = case getTimeout args of
                                Just n  -> (read n :: Timeout) * 1000  -- convert to milliseconds
                                Nothing -> 10000
                
        "--file"       : xs -> makeAbsolute (head xs) >>= launchOnFile
        _ -> showUsage

getTimeout :: [String] -> Maybe String
getTimeout [] = Nothing
getTimeout (x:xs) | x == "--timeout" = Just (head xs)
                  | otherwise = getTimeout xs


-- | Clean up and restore backups if program is interrupted.
interrupt :: ThreadId   -- ^ ThreadId to interrupt
          -> FilePath   -- ^ Project directory path
          -> IO ()
interrupt tid projDir = do
    srcDir <- srcDirFromProject projDir

    threadDelay 1000

    putStrLn "Interrupted, restoring files..."

    -- Remove MutateInject.hs and restore original cabal file.
    cleanMutateInject srcDir
    cabalPathFromProject projDir >>= \x -> restoreOriginal backupDir x

    -- Restore source files
    let srcBackup = P.Path (backupDir </> (takeFileName srcDir))
    let origLoc = P.Path (projDir </> (takeFileName srcDir))
    copyDirRecur srcBackup origLoc

    killThread tid

    where backupDir = projDir </> backupSuffix

showUsage :: IO ()
showUsage = do 
    putStrLn "Usage: haskell-mutate2-exe [OPTION]..."
    putStrLn "--help                    Shows this text."
    putStrLn "--project-dir DIRECTORY   Specifies directory of cabal project."
    putStrLn "                          Should contain a .cabal file and "
    putStrLn "                          defaults to ./ if not specified"
    putStrLn "--timeout NUMBER          Specifies the maximum time in ms to run"
    putStrLn "                          each test before timing out. Defaults to"
    putStrLn "                          10 000 (10 seconds)."
    putStrLn "--file FILE               (For debugging) Mutates a file and prints"
    putStrLn "                                          the mutations."


launchAtProject :: Timeout -> FilePath -> IO ()
launchAtProject to projectPath = do
    tid <- myThreadId
    installHandler keyboardSignal (Catch (interrupt tid projectPath)) Nothing

    -- Make backup of original source files
    srcDir <- srcDirFromProject projectPath
    copyDirRecur (P.Path srcDir) (P.Path $ backupDir </> (takeFileName srcDir))

    -- Check if there already exists a "MutateInject.hs", since we do not 
    -- want to overwrite the existing file in the off-chance that it isn't
    -- our injected file.
    checkInjectExists

    -- Build and run the unmodified test suites to see that they work when 
    -- unmodified.
    cabalBuildTargetsShow projectPath []
    putStrLn ""

    -- Clean old output folder
    wipeDirIfExists outputDir
    
    -- Get absolute file paths to all files in the source folder, these are 
    -- the files that will be mutated.
    -- NOTE: This needs to run BEFORE injecting MutateInject.hs
    srcFiles <- srcDirFromProject projectPath
                    >>= filePathsFromDir
                    >>= filterM (\x -> pure $ takeExtension x == ".hs")

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
    rs <- mapM (\x -> runRoutine to x projectPath) srcFiles
    let resultSummary = foldl (|+|) emptyRes rs

    -- Remove the injected MutateInject.hs file and restore the original 
    -- .cabal file.
    srcDirFromProject projectPath >>= cleanMutateInject
    cabalPathFromProject projectPath >>= \x -> restoreOriginal backupDir x

    printResults resultSummary
    
    where backupDir = projectPath </> backupSuffix
          outputDir = projectPath </> outputSuffix
          checkInjectExists = do
              putStrLn "==> Looking for conflicting MutateInject.hs..."
              srcDir <- srcDirFromProject projectPath
              exists <- doesFileExist $ srcDir </> "MutateInject.hs"
              when exists $ Prelude.error "MutateInject.hs already exists in \
                                          \source directory! Aborting."


-- | Given a path to a file, project, and a TestSummary, runs the process of 
--   backing up the original file, mutating it, testing it, and restoring the 
--   original file back. Then returns the updated TestSummary.
runRoutine :: Timeout -> FilePath -> FilePath -> IO ResultSummary
runRoutine to filePath projectPath = do
    putStrLn $ "==> Performing tests on mutant: " ++ takeFileName filePath

    -- Back up the original file to the backup directory, creating the backup
    -- directory if not already existing.
    backupOriginal filePath backupDir

    -- Create mutated ASTs of the target file.
    mutantModules <- mutateFile filePath

    -- Run the tests with the mutant, return the results from the test
    testSummary <- runTestsWithMutants filePath to mutantModules projectPath emptyRes

    -- Restore the original file from the backup location
    restoreOriginal backupDir filePath

    return testSummary

    where backupDir = projectPath </> backupSuffix
          outputDir = projectPath </> outputSuffix


-- | Performs tests in a target directory with all generated mutants of a 
--   specified source file. Then returns an updated ResultSummary from the 
--   input summary.
runTestsWithMutants :: FilePath             -- ^ Path to source file to mutate.
                    -> Timeout              -- ^ Time that each test has before timing out
                    -> [Module SrcSpanInfo] -- ^ List of mutant ASTs.
                    -> FilePath             -- ^ Path to project directory.
                    -> ResultSummary
                    -> IO ResultSummary     -- ^ Result summary of test runs.
runTestsWithMutants _ _ [] _ testSum = return testSum
runTestsWithMutants filePath to (m:ms) projectPath testSum = do
    -- Remove old file and write mutant to file.
    insertMutant

    putStrLn $ "\n==> Building and testing mutant... (" 
              ++ show (length $ m:ms) ++ " left)"

    -- Build the tests, returning its results, then runs the tests if building
    -- was successful. If building failed, then the error count of the test 
    -- summary is incremented. Depends on the test results otherwise.

    -- The reason for first building the tests is to separate build errors 
    -- from failed tests in the testing section (and possibly warnings)
    res <- cabalBuildTargets projectPath 
                =<< testSuitesFromProject projectPath
    case res of
        (ExitFailure n, _, stderr) -> do
            putStrLn $ "Build failed with an ERROR \
                       \(exit code " ++ show n ++ "):"
            putStrLn stderr
            let newSum = incError testSum

            runTestsWithMutants filePath to ms projectPath newSum

        (ExitSuccess, _, _) -> do
            putStrLn "Build succeeded."

            -- Run tests with a process, returning its results
            exitStatus <- withCurrentDirectory projectPath $
                            readProcessWithExitCode "cabal" ["test"] ""
            let testResult = getTestResult exitStatus
            
            -- Perform actions depending on the results
            handleTestResult testResult m filePath (projectPath </> outputSuffix)

            -- Update the summary values depending on the results
            let newSum = updateSummary testSum testResult

            runTestsWithMutants filePath to ms projectPath newSum

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

        putStrLn "Tests succeeded; mutant SURVIVED.\n"
        putStrLn   "==> The mutant that was being tested was:"
        putStrLn $ "==> " ++ mutantPath

        -- Write surviving mutant to file
        writeFile newMutantPath $ prettyPrint mutant
        putStrLn $ "Copy of mutant saved to: " ++ newMutantPath

        -- For debugging, mostly:
        {- 
        putStrLn "==> The mutant looked like:"
        putStrLn $ prettyPrint mutant
        putStrLn ""
        -}


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


-- | For debugging purposes. Performs mutations on a single file and 
--  prettyPrints the results.
launchOnFile :: FilePath -> IO ()
launchOnFile p = do
    ms <- mutateFile p
    mapM_ printMutantD ms
    putStrLn $ "Total number of mutants: " ++ show (length ms)


printMutantD :: Module SrcSpanInfo -> IO ()
printMutantD m = do
    putStrLn "\n====> MUTANT START\n"
    putStrLn $ prettyPrint m
    putStrLn ""
