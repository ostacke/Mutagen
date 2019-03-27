module Mutate
    ( mutate
    ) where

import GHC.Float
import Language.Haskell.Exts
import Data.List

-- | Defining a class Mutable. The function mutate takes a member of the 
--   class as an argument, and the result is of the same type. (?)
class Mutable a where
    mutate :: a -> [a]

m1 :: (Mutable a) => (a -> b) -> a -> [b]
m1 f a = aMutants
    where aMutant = mutate a
          aMutants = map (\x -> f x) aMutant

-- | Returns the list of mutants created by applying mutate on types
--   with two type parameters (excluding location l)
m2 :: (Mutable a, Mutable b) => (a -> b -> c) -> a -> b -> [c]
m2 f a b = aMutants ++ bMutants
    where aMutant = mutate a
          bMutant = mutate b
          aMutants = map (\x -> f x b) aMutant
          bMutants = map (\x -> f a x) bMutant

-- | Same as m2, but with three parameters
m3 :: (Mutable a, Mutable b, Mutable c) =>
    (a -> b -> c -> d) -> a -> b -> c -> [d]
m3 f a b c = aMutants ++ bMutants ++ cMutants
    where aMutant = mutate a
          bMutant = mutate b
          cMutant = mutate c
          aMutants = map (\x -> f x b c) aMutant
          bMutants = map (\x -> f a x c) bMutant
          cMutants = map (\x -> f a b x) cMutant

-- | Same as m2, but with four parameters
m4 :: (Mutable a, Mutable b, Mutable c, Mutable d) =>
    (a -> b -> c -> d -> e) -> a -> b -> c -> d -> [e]
m4 f a b c d = aMutants ++ bMutants ++ cMutants ++ dMutants
    where aMutant = mutate a
          bMutant = mutate b
          cMutant = mutate c
          dMutant = mutate d
          aMutants = map (\x -> f x b c d) aMutant
          bMutants = map (\x -> f a x c d) bMutant
          cMutants = map (\x -> f a b x d) cMutant
          dMutants = map (\x -> f a b c x) dMutant

-- | Same as m2, buth with five parameters
m5 :: (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e) =>
    (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> [f]
m5 f a b c d e = aMutants ++ bMutants ++ cMutants ++ dMutants ++ eMutants
    where aMutant = mutate a
          bMutant = mutate b
          cMutant = mutate c
          dMutant = mutate d
          eMutant = mutate e
          aMutants = map (\x -> f x b c d e) aMutant
          bMutants = map (\x -> f a x c d e) bMutant
          cMutants = map (\x -> f a b x d e) cMutant
          dMutants = map (\x -> f a b c x e) dMutant
          eMutants = map (\x -> f a b c d x) eMutant

instance (Show a) => Mutable (Module a) where
    mutate mod = case mod of
        Module l mbyHead pragmas importDecls decls -> 
            mod : m4 (Module l) mbyHead pragmas importDecls decls
        -- Returns the original module concatenated by all the variations,
        -- helps with output later.

{-  ModuleHead includes the name and export information. Should
    probably remain unmutated.
-}
instance Mutable (ModuleHead a) where
    mutate rest = []

{-  ModulePragma contains messages to the compiler. Should probably
    remain unmutated.
-}
instance Mutable (ModulePragma a) where
    mutate rest = []

{-  ImportDecl contains import declarations. Should probably remain
    unmutated.
-}
instance Mutable (ImportDecl a) where
  mutate rest = []

{-  Decl is the top level of all the declarations and functions
    in a Haskell file.
    Mutated: Operator fixity, function binding clauses and pattern bindings
-}
instance Mutable (Decl a) where
    mutate decl = case decl of
        TypeDecl l declHead typ -> []
        TypeFamDecl l declHead resultSig injectivityInfo -> []
        ClosedTypeFamDecl l declHead resultSig injectivityInfo typeEqn -> []
        DataDecl l dataOrNew context declHead qualConDecl derivin -> []
        GDataDecl l dataOrNew context declHead kind gadtDecl derivin -> []
        DataFamDecl l context declHead resultSig -> []
        TypeInsDecl l typ1 typ2 -> []
        DataInsDecl l dataOrNew typ qualConDecl derivin -> []
        GDataInsDecl l dataOrNew typ kind gadtDecl derivin -> []
        ClassDecl l context declHead funDep classDecl -> []
        InstDecl l overlap instRule instDecl
            -> m3 (InstDecl l) overlap instRule instDecl
        DerivDecl l derivStrategy overlap instRule -> []
        InfixDecl l assoc int op
            -> mutantsA ++ mutantsB
                where
                mutantsA = map(\x -> InfixDecl l x int op) (mutate assoc)
                mutantsB = map(\x -> InfixDecl l assoc x op) (precMutate)
                precMutate = map Just [0..9]
        DefaultDecl l typ -> []
        SpliceDecl l exp
            -> m1 (SpliceDecl l) exp
        TypeSig l name typ -> []
        PatSynSig l name tyVarBind1 context1 tyVarBind2 context2 typ -> []
        FunBind l match -> m1 (FunBind l) match
        PatBind l pat rhs binds
            -- -> m3 (PatBind l) pat rhs mbyBinds
            -> mutantsA ++ mutantsB ++ mutantsC
                where
                mutantsA = map (\x -> PatBind l x rhs mbyBinds) (mutate pat)
                mutantsB = map (\x -> PatBind l pat x mbyBinds) (mutate rhs)
                mutantsC = map (\x -> PatBind l pat rhs x) bindsMutate
                bindsMutate = [x | x <- (mutate binds), x !! Nothing]
        PatSyn l pat1 pat2 patternSynDirection
            -> m3 (PatSyn l) pat1 pat2 patternSynDirection --TODO, vad är skillnaden mellan en pattern synonym och en pattern synonym signature declaration?
        ForImp l callConv safety string name typ -> []
        ForExp l callConv string name typ -> []
        RulePragmaDecl l rule -> []
        DeprPragmaDecl l nameString -> []
        WarnPragmaDecl l nameString -> []
        InlineSig l bool activation qName -> []
        InlineConlikeSig l activation qName -> []
        SpecSig l activation qName typ -> []
        SpecInlineSig l bool activation qName typ -> []
        InstSig l instRule -> []
        AnnPragma l booleanFormula -> []
        MinimalPragma l booleanFormula -> []
        RoleAnnotDecl l qName role -> []
        CompletePragma l name qName -> []
        _ -> []

{- Det här borde man kunna göra snyggare..
-}
instance Mutable (Assoc a) where
    mutate (AssocNone l)  = [AssocNone l, AssocLeft l, AssocRight l]
    mutate (AssocLeft l)  = [AssocNone l, AssocLeft l, AssocRight l]
    mutate (AssocRight l) = [AssocNone l, AssocLeft l, AssocRight l]

instance Mutable (Op a) where
    mutate _ = []

instance Mutable Int where
    mutate n = [0, 1, n+1, n-1, n*(-1)]

{- Borde inte muteras
-}
instance Mutable (Overlap a) where
    mutate _ = []

{- Borde inte muteras
-}
instance Mutable (InstRule a) where
    mutate _ = []

instance Mutable (InstDecl a) where
    mutate instDecl = case instDecl of    
        InsDecl l decl -> m1 (InsDecl l) decl
        _ -> []

{- TODO
-}
instance Mutable (Alt a) where
    mutate _ = []

instance Mutable (Rhs a) where
    mutate (UnGuardedRhs l exp) = m1 (UnGuardedRhs l) exp

    mutate (GuardedRhss l guardedRhss) = m1 (GuardedRhss l) guardedRhss

    mutate _ = []

instance Mutable (GuardedRhs a) where
    mutate (GuardedRhs l stmts exp) = m2 (GuardedRhs l) stmts exp

{- TODO
-}
instance Mutable (Stmt a) where
    mutate _ = []

instance Mutable (Binds a) where
    mutate (BDecls l decls) = m1 (BDecls l) decls
    mutate _ = []
    -- TODO: mutate (IPBinds l ipbinds) = m1 (BDecls l) ipbinds

instance Mutable (Match a) where
    mutate match = case match of
        Match l name pat rhs mbyBinds -> m4 (Match l) name pat rhs mbyBinds
        _ -> []

instance Mutable (Name a) where
    mutate name = case name of
        Symbol l s -> [(Symbol l "-")]

        _ -> []

instance Mutable (Pat a) where
    mutate pat = case pat of
        _ -> []


instance Mutable (Exp a) where
    mutate (InfixApp l e1 qOp e2)       = m3 (InfixApp l) e1 qOp e2
    mutate (If l ifExp thenExp elseExp) = m3 (If l) ifExp thenExp elseExp
    mutate (Lit l literal)              = m1 (Lit l) literal
    mutate _ = []

instance Mutable (QOp a) where
    mutate (QVarOp l qName) = m1 (QVarOp l) qName
    mutate rest             = []

instance Mutable (QName a) where
    mutate (UnQual l name) = m1 (UnQual l) name
    mutate rest = []

instance Mutable (Literal a) where
    mutate (Int l int _)  = mapLit (Int l) int (intMuts int)
        where intMuts n = [0, 1, n+1, n-1, n*(-1)]

    mutate (Frac l rat _) = mapLit (Frac l) rat (ratMuts rat)
        where ratMuts r = [0, 1, r+1, r-1, r*(-1), r/2, r*2, r/10, r*10,
                           (r * 2)/2, (r/2)*2, 
                           fromIntegral $ truncate r,
                           fromIntegral $ floor r, 
                           fromIntegral $ ceiling r, 
                           fromIntegral $ round r]

    mutate (Char l char _) = mapLit (Char l) char (charMuts char)
        where charMuts c = [succ c, pred c, 'x', 'a']

    mutate (String l str _) = mapLit (String l) str (strMuts str)
        where strMuts xs = [' ':xs, tail xs, reverse xs, init xs, xs ++ " "]

    -- TODO: Unboxed literals(?)

    mutate _ = []
-- Helper function for mutate on Literals
mapLit constr param xs = map (\x -> constr x (show x)) xs

{- Borde nog inte muteras
-}
instance Mutable (PatternSynDirection a) where
    mutate _ = []

instance (Mutable a) => Mutable (Maybe a) where
    mutate (Just a) = Nothing : m1 Just a
    mutate _        = []

instance (Mutable a) => Mutable [a] where
    mutate []     = []
    mutate (x:xs) = m2 (:) x xs


