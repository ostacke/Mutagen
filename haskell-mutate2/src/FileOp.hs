module FileOp
    ( wipeDirIfExists
    ) where

import System.Directory
import System.FilePath

-- | Deletes directory (recursively) if it exists and replaces it with a new,
--   empty directory. Creates a new directory if it does not exist.
wipeDirIfExists :: FilePath -> IO ()
wipeDirIfExists dir = do
    exists <- doesDirectoryExist dir
    if exists then do removeDirectoryRecursive dir
                      createDirectory dir
              else createDirectory dir