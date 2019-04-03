{-# LANGUAGE TemplateHaskell #-}

module FileOp
    ( backupOriginal
    , cabalAddModule
    , cleanMutateInject
    , copyMutateInject
    , restoreOriginal
    , wipeDirIfExists
    ) where

import Data.ByteString as BS (writeFile)
import Data.FileEmbed
import Data.Char
import System.Directory
import System.FilePath
import System.IO


-- | Adds module to the target .cabal file's "other-modules" field
--   to suppress warnings.
cabalAddModule :: String   -- ^ Name of module to insert
               -> FilePath -- ^ Path to .cabal file
               -> IO ()
cabalAddModule m p = do
    contents <- readFile p
    let newContent = unlines $ cabalAddModule' "library" m $ lines contents
    Prelude.writeFile tmp newContent
    removeFile p
    renameFile tmp p
    where tmp = p ++ ".tmp"

cabalAddModule' _ _ [] = error "Couldn't find field to insert module."
cabalAddModule' target m (x:xs)
    | isTarget x = x : cabalAddModule'' "other-modules:" m xs
    | otherwise  = x : cabalAddModule' target m xs
    where isTarget x = take (length target) (dropWhile isSpace x) == target

cabalAddModule'' _ _ [] = error "Couldn't find field to insert module."
cabalAddModule'' target m (x:xs)
    | isTarget x = x : toInsert : xs
    | otherwise  = x : cabalAddModule'' target m xs
    where isTarget x  = take (length target) (dropWhile isSpace x) == target
          toInsert = "      " ++ m -- To match indentation of existing stuff


-- | Writes the MutateInject.hs file to the target project's src folder.
copyMutateInject :: FilePath -> IO ()
copyMutateInject srcDir = do
    -- Embed contents of MutateInject as ByteString
    let moduleContents = $(embedFile "./src/MutateInject.hs")

    BS.writeFile (srcDir </> fileName) moduleContents

    where fileName = takeFileName "./src/MutateInject.hs"

-- | Removes MutateInject.hs from the target directory.
cleanMutateInject :: FilePath -> IO ()
cleanMutateInject dir = do 
    mbyFP <- findFile [dir] "MutateInject.hs"
    case mbyFP of
        Just path -> withCurrentDirectory dir $ removeFile path
        Nothing -> putStrLn "Could not find file 'MutateInject.hs' to clean."

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
