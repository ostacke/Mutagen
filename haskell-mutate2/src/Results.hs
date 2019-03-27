module Results where

import System.Exit

data ResultSummary = ResultSummary
    { survived :: Int -- ^ Number of mutants that survived tests.
    , killed   :: Int -- ^ Number of mutants that were killed by tests.
    , error    :: Int -- ^ Number of mutants that caused errors when testing.
    } deriving (Show)

incSurvived (ResultSummary s f e) = ResultSummary (s + 1) f e
incKilled (ResultSummary s f e) = ResultSummary s (f + 1) e
incError  (ResultSummary s f e) = ResultSummary s f (e + 1)
                

-- | Adds two ResultSummaries by adding their respective fields.
(|+|) :: ResultSummary -> ResultSummary -> ResultSummary
(ResultSummary s1 k1 e1) |+| (ResultSummary s2 k2 e2) =
    ResultSummary (s1 + s2) (k1 + k2) (e1 + e2)

printResults :: ResultSummary -> IO ()
printResults (ResultSummary s k e) = do
    putStrLn ":: SUMMARY ::"
    putStrLn $ "In total, " ++ show (s + k + e) ++ " mutants were created."
    putStrLn $ "Number of mutants killed: " ++ show k
    putStrLn $ "Number of mutants that survived: " ++ show s
    putStrLn $ "Number of errors: " ++ show e


updateSummary :: ResultSummary -> TestResult -> ResultSummary
updateSummary summary res = case res of
    Survived _ -> incSurvived summary
    Killed _   -> incKilled summary
    Error _    -> incError summary


-- | Data type representing how a mutant fared during a test suite, along with
--   the output of stdout or stderr, depending on the result.
data TestResult = Survived String   -- ^ Mutant survived the tests, stdout
                | Killed String     -- ^ Tests killed the mutant, stdout
                | Error String      -- ^ Tests threw an exception, stderr


getTestResult :: (ExitCode, String, String) -> TestResult
getTestResult (ExitSuccess, stdout, _) = Survived stdout
getTestResult (ExitFailure exitCode, stdout, stderr)
    | null stderr = Killed stdout
    | otherwise = Error stderr
