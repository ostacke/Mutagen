module FileOp
    ( wipeDirIfExists
    , backupOriginal
    , restoreOriginal
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


-- | Functions for backing up original files and restoring them from backup.
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
