module Main where

import System.Environment

import Text.Pretty.Simple
import Language.Haskell.Exts

main :: IO ()
main = do
    args <- getArgs

    case length args of
        1 -> run (head args)
        _ -> showUsage

showUsage = putStrLn $ "Usage: hse-better-print-exe FILE"

run :: String -> IO ()
run path = do
    res <- parseFile path
    
    case res of
        ParseOk m -> output m path
        otherwise -> putStrLn $ "Parsing failed."

output :: Module SrcSpanInfo -> String -> IO ()
output m path = do
    pPrintNoColor m

    putStrLn $ ""
    putStrLn $ "Input (\"pretty\"): "
    putStrLn $ ""
    putStrLn $ prettyPrint m
    putStrLn $ ""    

    {- Currently doesn't show up correctly when cat-ing output file -}
    putStrLn $ "Writing output to file: " ++ newPath
    writeFile newPath $ show (pShowNoColor m)
    putStrLn $ "Finished writing to file."
    
        where newPath = path ++ ".pretty.hs"
    
