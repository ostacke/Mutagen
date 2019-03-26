module Results where

data ResultSummary = ResultSummary
    { survived :: Int -- ^ Number of mutants that survived tests.
    , killed   :: Int -- ^ Number of mutants that were killed by tests.
    , error      :: Int -- ^ Number of mutants that caused errors when testing.
    } deriving (Show)

-- | Data type representing how a mutant fared during a test suite, along with
--   the output of stdout or stderr, depending on the result.
data TestResult = Survived String   -- ^ Mutant survived the tests, stdout
                | Killed String     -- ^ Tests killed the mutant, stdout
                | Error String      -- ^ Tests threw an exception, stderr

incSurvived (ResultSummary s f e) = ResultSummary (s + 1) f e
incKilled (ResultSummary s f e) = ResultSummary s (f + 1) e
incError  (ResultSummary s f e) = ResultSummary s f (e + 1)

(|+|) :: ResultSummary -> ResultSummary -> ResultSummary
(ResultSummary s1 k1 e1) |+| (ResultSummary s2 k2 e2) =
    ResultSummary (s1 + s2) (k1 + k2) (e1 + e2)

    
    