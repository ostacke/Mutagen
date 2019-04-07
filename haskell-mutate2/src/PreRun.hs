module PreRun where

import System.Directory

-- | Runs `cabal build` in the target directory.
cabalBuild :: FilePath -> IO ExitCode
cabalBuild path = return $
    withCurrentDirectory path $ readProcessWithExitCode "cabal" ["build"] ""


-- | Runs `cabal build` on a list of target modules.
cabalBuildTargets :: FilePath -> [String] -> IO ExitCode
cabalBuildTargets path targets = return $
    withCurrentDirectory path $ 
        readProcessWithExitCode "cabal" ("build" : targets) ""
