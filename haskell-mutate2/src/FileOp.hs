{-# LANGUAGE TemplateHaskell #-}

module FileOp
    ( wipeDirIfExists
    , backupOriginal
    , restoreOriginal
    , copyMutateInject
    ) where

import Data.FileEmbed
import Data.ByteString as BS (writeFile)
import System.Directory
import System.FilePath


-- | Writes the MutateInject.hs file to the target project's src folder.
copyMutateInject :: FilePath -> IO ()
copyMutateInject srcDir = do
    -- Embed contents of MutateInject as ByteString
    let moduleContents = $(embedFile "./src/MutateInject.hs")

    BS.writeFile (srcDir </> fileName) moduleContents

    where fileName = takeFileName "./src/MutateInject.hs"


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
