module Main where

import System.Environment
import System.Directory

import Language.Haskell.Exts

import Mutate

main :: IO ()
main = do
    args <- getArgs

    case args of
        "--help"    : xs    -> showUsage
        "--version" : xs    -> putStrLn version
        _                   -> launch args

version :: String
version = "haskell-mutate2 version 0.1.0.0"

showUsage :: IO ()
showUsage = do
    putStrLn "haskell-mutate2 version 0.1.0.0"
    putStrLn "Usage: haskell-mutate2 SOURCE"

launch :: [String] -> IO ()
launch args = case length args of
    0 -> showUsage
    1 -> do
        putStrLn $ ""
        putStrLn $ "haskell-mutate2 version 0.1.0.0"
        putStrLn $ "Attempting to parse and mutate file..."
        putStrLn $ "" 

        mutateOnPath (head args)

    _ -> showUsage

mutateOnPath :: String -> IO ()
mutateOnPath path = do
    res <- parseFile path
    case res of
        ParseOk ast -> do
            putStrLn $ "Parsing successful, creating mutants..."
            let mutantTrees = mutate ast
            
            putStrLn $ "Mutants created, writing to output files..."
            writeMutants path mutantTrees

            putStrLn $ "Finished writing mutants to files."

        ParseFailed l errMsg -> do
            putStrLn $ "Parsing failed:"
            putStrLn $ ""
            putStrLn $ errMsg

writeMutants :: String -> [Module l] -> IO ()
writeMutants _ []        = return ()
writeMutants path (x:xs) = do
    let outputDir = "out"
    createDirectoryIfMissing True outputDir
    
    let mutantPath = path ++ "-mutant-" ++ show (length xs) ++ ".hs"
    withCurrentDirectory outputDir $ writeFile mutantPath (prettyPrint x)
    
    writeMutants path xs

printOutput :: String -> IO ()
printOutput output = do
    putStrLn $ "Parsing finished, mutation created:"
    putStrLn $ ""
    putStrLn $ output
    putStrLn $ ""

saveOutput :: String -> String -> IO ()
saveOutput output path = do
    putStrLn $ "Saving mutant to file..."
    
    let mutantPath = path ++ "-mutant.hs"
    writeFile mutantPath output
    
    putStrLn $ "Mutant saved to: " ++ mutantPath
    putStrLn $ ""

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
    -- TypeSig l names types -> TypeSig l (mutateNames names) types
    -- FunBind l matches -> FunBind l (mutateMatches matches)
    _ -> decl

mutateMatches :: [Match l] -> [Match l]
mutateMatches []     = []
mutateMatches (x:xs) = case x of
    -- Match l n ps rhs mb -> Match l n ps (mutateRhs rhs) mb : xs
    _ -> undefined -- if InfixMatch

mutateRhs :: Rhs l -> Rhs l
mutateRhs rhs = case rhs of
    -- UnGuardedRhs l exp -> UnGuardedRhs l (mutateExp exp)
    _ -> undefined -- if GuardedRhs

mutateExp :: Exp l -> Exp l
mutateExp exp = case exp of
    -- Lit l literal -> Lit l (mutateLiteral literal)
    -- InfixApp l e1 qop e2 -> InfixApp l e1 (mutateQOp qop) (mutateExp e2)
    _ -> exp

mutateQOp :: QOp l -> QOp l
mutateQOp q = case q of
    -- QVarOp l qn -> QVarOp l (mutateQName qn)
    -- QConOp l qn -> QConOp l (mutateQName qn)
    _ -> q

mutateQName :: QName l -> QName l
mutateQName n = case n of
    -- Qual l m n -> Qual l m (mutate n)
    -- UnQual l n -> UnQual l (mutate n)
    _ -> n --SpecialCon

mutateLiteral :: Literal l -> Literal l
mutateLiteral lit = case lit of
    -- Int l int str -> Int l mutatedInt (show mutatedInt)
    --     where mutatedInt = mutate int
    _ -> lit

mutateNames :: [Name l] -> [Name l]
mutateNames names = names
--mutateNames []     = []
--mutateNames (n:ns) = mutate n : mutateNames ns

mutateName :: Name l -> Name l
mutateName name = case name of
    -- Ident l id  -> Ident l id
    -- Symbol l id -> Symbol l "TESTSYMBOL"
    _ -> name


