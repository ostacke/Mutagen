module Helper.PreRun where

import System.Exit
import System.Directory
import System.Process

import Helper.Results


-- | Runs `cabal build` on a list of target modules inside a specified
--   directory.
cabalBuildTargets :: FilePath -> [String] -> IO (ExitCode, String, String)
cabalBuildTargets path targets = withCurrentDirectory path $ 
    readProcessWithExitCode "cabal" ("build" : targets) ""


-- | Runs cabalBuild (optionally with targets) and presents the results.
cabalBuildTargetsShow :: FilePath -> [String] -> IO (ExitCode, String, String)
cabalBuildTargetsShow path targets = do
    putStrLn "==> Building tests on the ORIGINAL code..."
    res <- cabalBuildTargets path targets
    case res of
        (ExitSuccess, stdin, stderr) -> return res

        (ExitFailure n, stdin, stderr) -> do
            putStrLn $ "Error occurred when building tests for the original \
                       \code (exit code " ++ show n ++ "):"
            putStrLn stderr
            return res