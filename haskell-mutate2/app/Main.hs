module Main where

import System.Environment
import Language.Haskell.Exts

main :: IO ()
main = do
    args <- getArgs

    case args of
        ("--print" : args')   -> printParseRes $ head args'
        ["--help"]            -> showUsage
        ["--version"]         -> putStrLn shortVersion
        _                     -> launch args

launch args = do
    if length args == 0
        then showUsage
        else mutato (head args)

shortVersion :: String
shortVersion = "haskell-mutate2 version 0.1.0.0"

showUsage :: IO ()
showUsage = do
    putStrLn "haskell-mutate2 version 0.1.0.0"
    putStrLn "Usage: haskell-mutate2 SOURCE"

printParseRes :: String -> IO ()
printParseRes path = do
    res <- parseFile path
    case res of
        ParseOk (Module _ _ _ _ decls) -> dissectShow $ decls
        otherwise -> putStrLn $ "Parsing failed."

dissectShow :: [Decl a] -> IO ()
dissectShow []     = print ""
dissectShow (x:xs) = do
    print "UNIMPLEMENTED"
    dissectShow xs


mutato :: String -> IO ()
mutato path = do
    putStrLn $ ""
    putStrLn $ "haskell-mutate2 version 0.1.0.0"
    putStrLn $ "Attempting to parse and mutate file..."
    putStrLn $ "" 

    res <- parseFile path
    case res of
        ParseOk m -> do 
            putStrLn $ prettyPrint $ handleModule m
            putStrLn $ ""
            putStrLn $ "Parsing finished, mutation created."
        otherwise -> putStrLn $ "Parsing failed."

-- | Thus far only looks at the declarations of the body, ignoring the 
--   imports and other stuff, will probably mess the prettyPrint up later.
handleModule :: (Module SrcSpanInfo) -> (Module SrcSpanInfo)
handleModule (Module l h p i decls) = (Module l h p i (mutateBody decls))

-- | This looks at a list of declarations in the file and does stuff
mutateBody :: [Decl l] -> [Decl l]
mutateBody []     = []
mutateBody (x:xs) = mutateDecl x : mutateBody xs

mutateDecl :: Decl a -> Decl a
mutateDecl decl = case decl of
    TypeSig l names types -> TypeSig l (mutateNames names) types
    otherwise -> decl

mutateNames :: [Name l] -> [Name l]
mutateNames []     = []
mutateNames (n:ns) = mutateName n : mutateNames ns

mutateName :: Name l -> Name l
mutateName name = case name of
    Ident l id  -> Ident l "subtract"
    Symbol l id -> Symbol l "TESTSYMBOL"

