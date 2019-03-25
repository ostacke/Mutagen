-- Module containing different operations on paths and directories.
-- Functions in this module should not modify anything in the filesystem,
-- look to the FileOp module for such functions.

module Path 
    ( srcDirFromProject
    , getAbsoluteDirContents
    , filePathsFromDir
    ) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath


-- | Given the path to a directory, returns a list of the file paths to all 
--   the files in the directory and its subdirectories.
filePathsFromDir :: FilePath -> IO [FilePath]
filePathsFromDir dir = do
    contents <- getAbsoluteDirContents dir
    dirs <- filterM doesDirectoryExist contents
    files <- filterM doesFileExist contents

    r <- mapM filePathsFromDir dirs
    let recursiveFiles = concat r

    return $ files ++ recursiveFiles


-- | Given the path to a directory containing a .cabal file,
--   returns the source directory for the library modules.
srcDirFromProject :: FilePath -> IO FilePath
srcDirFromProject projectDir = do
    cabalContents <- readFile =<< cabalPathFromProject projectDir
    return $ projectDir </> srcDirFromCabal cabalContents


-- | Given a path to a directory, returns the path to the .cabal file
--   Throws an exception in the fromJust otherwise.
cabalPathFromProject :: FilePath -> IO FilePath
cabalPathFromProject projectDir = do
    contents <- getAbsoluteDirContents projectDir
    return $ fromJust $ find (\x -> takeExtension x == ".cabal") contents


-- Given the contents of a .cabal file, returns the name of the 
-- "hs-source-dirs" field (the src directory).
srcDirFromCabal :: String -> String
srcDirFromCabal contents = findLibrary $ lines contents
findLibrary [] = "src"
findLibrary (x:xs) | null x = findLibrary xs
                   | head x == ' '  = findLibrary xs
                   | x == "library" = findSrc xs
                   | otherwise = findLibrary xs
findSrc [] = "src"
findSrc (x:xs) 
    | dropWhile isSpace x == "hs-source-dirs:" = dropWhile isSpace (head xs)
    | otherwise = findSrc xs 


-- | Given a path to a directory, returns a list of the absolute paths for 
--   every file at the directory.
getAbsoluteDirContents :: FilePath -> IO [FilePath]
getAbsoluteDirContents dir = do
    contents <- listDirectory dir
    let relativePaths = map (dir </>) contents
    mapM canonicalizePath relativePaths

