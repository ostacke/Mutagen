module Mutate
    ( mutate
    , safeMutate
    , injectMutateInject
    ) where

import GHC.Float
import Language.Haskell.Exts
import Data.List
import Data.Maybe


-- | Defining a class Mutable. The function mutate takes a member of the 
--   class as an argument, and the result is of the same type. (?)
class Mutable a where
    mutate :: a -> [a]

-- | Removes mutations identical to the original input
safeMutate :: (Mutable a, Eq a) => a -> [a]
safeMutate a = filter (/= a) $ nub $ mutate a

-- | Adds "import MutateInject" to the import declarations of a Module
injectMutateInject :: Module SrcSpanInfo -> Module SrcSpanInfo
injectMutateInject m = case m of
    Module l mb ps imports ds -> Module l mb ps (injectedImport : imports) ds
        where injectedImport = ImportDecl 
                                { importAnn = l
                                , importModule = ModuleName l "MutateInject"
                                , importQualified = False
                                , importSrc = False
                                , importSafe = False
                                , importPkg = Nothing
                                , importAs = Nothing
                                , importSpecs = Nothing } 

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
        Module l (Just moduleHead) pragmas importDecls decls -> 
            mod : m3 (Module l (Just moduleHead)) pragmas importDecls decls
        -- Returns the original module concatenated by all the variations,
        -- helps with output later. Also retains the modulehead
        Module l mbyHead pragmas importDecls decls ->
            mod : m4 (Module l) mbyHead pragmas importDecls decls

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
                mutantsB = map(\x -> InfixDecl l assoc x op) precMutate
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
                mutantsA = map (\x -> PatBind l x rhs binds) (mutate pat)
                mutantsB = map (\x -> PatBind l pat x binds) (mutate rhs)
                mutantsC = map (\x -> PatBind l pat rhs x) bindsMutate
                bindsMutate = [x | x <- (mutate binds), isJust x]
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
        -- _ -> []

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

instance Mutable (Rhs a) where
    mutate (UnGuardedRhs l exp) = m1 (UnGuardedRhs l) exp
    mutate (GuardedRhss l guardedRhss) = m1 (GuardedRhss l) guardedRhss

instance Mutable (GuardedRhs a) where
    mutate (GuardedRhs l stmts exp) = m2 (GuardedRhs l) stmts exp

instance Mutable (Stmt a) where
    mutate stmt = case stmt of
        Generator l pat exp -> m2 (Generator l) pat exp
        Qualifier l exp     -> m1 (Qualifier l) exp
        LetStmt l binds     -> m1 (LetStmt l) binds
        RecStmt l stmts     -> m1 (RecStmt l) stmts

instance Mutable (Binds a) where
    mutate binds = case binds of
        BDecls l decls    -> m1 (BDecls l) decls
        IPBinds l ipbinds -> m1 (IPBinds l) ipbinds

instance Mutable (IPBind a) where
    mutate (IPBind l ipName exp) = map (IPBind l ipName) (mutate exp)

instance Mutable (Match a) where
    mutate match = case match of
        Match l name pat rhs mbyBinds -> m4 (Match l) name pat rhs mbyBinds
        _ -> []

instance Mutable (Name a) where
    mutate name = case name of
        Symbol l s -> map (Symbol l) $ eqMuts ++ ordMuts ++ intOpMuts ++ fracOpMuts
        _ -> []

        where eqMuts = ["==", "/="]
              ordMuts = ["<", ">", "<=", ">="]
              intOpMuts = ["+", "-", "*", "%"]
              fracOpMuts = ["/"]

instance Mutable (Pat a) where
  mutate (PVar l n)              = m1 (PVar l) n
  mutate (PLit l s li)           = [] -- m2 (PLit l) s li
  mutate (PNPlusK l n i)         = [] -- m2 (PNPlusK l) n i
  mutate (PInfixApp l p1 n p2)   = m3 (PInfixApp l) p1 n p2
  mutate (PApp l n p)            = m2 (PApp l) n p
  mutate (PTuple l b p)          = m2 (PTuple l) b p
  mutate (PUnboxedSum l i1 i2 p) = [] -- m3 (PUnboxedSum l) i1 i2 p
  mutate (PList l p)             = m1 (PList l) p
  mutate (PParen l p)            = m1 (PParen l) p
  mutate (PRec l n p)            = [] -- m2 (PRec l) n p
  mutate (PAsPat l n p)          = m2 (PAsPat l) n p
  mutate (PWildCard l)           = []
  mutate (PIrrPat l p)           = m1 (PIrrPat l) p
  mutate (PatTypeSig l p t)      = [] -- m2 (PatTypeSig l) p t
  mutate (PViewPat l e p)        = m2 (PViewPat l) e p
  mutate (PRPat l r)             = [] -- m1 (PRPat l) r
  mutate (PBangPat l p)          = m1 (PBangPat l) p


instance Mutable (Exp a) where
    mutate exp = case exp of

        Var l qn                      -> App l (mInject l) exp : m1 (Var l) qn
        OverloadedLabel l str         -> []
        IPVar l n                     -> []
        Con l n                       -> m1 (Con l) n
        Lit l literal                 -> m1 (Lit l) literal
        InfixApp l e1 qOp e2          -> App l (mInject l) exp : m3 (InfixApp l) e1 qOp e2
        App l e1 e2                   -> App l (mInject l) exp : m2 (App l) e1 e2
        NegApp l e                    -> e : mutate e ++ m1 (NegApp l) e
        Lambda l ps e                 -> m2 (Lambda l) ps e
        Let l b e                     -> m2 (Let l) b e
        If l ifE thenE elseE          -> If l ifE elseE thenE : m3 (If l) ifE thenE elseE
        MultiIf l gs                  -> map (MultiIf l) (guardMuts gs) ++ m1 (MultiIf l) gs
        Case l e as                   -> map (Case l e) (caseMuts as) ++ m2 (Case l) e as
        Do l ss                       -> m1 (Do l) ss -- The last statement in the list should be an expression.
        MDo l ss                      -> m1 (MDo l) ss
        Tuple l b es                  -> m2 (Tuple l) b es
        UnboxedSum l i1 i2 e          -> m3 (UnboxedSum l) i1 i2 e
        TupleSection l b mbyExp       -> m2 (TupleSection l) b mbyExp
        List l es                     -> map (List l) (listMuts es) ++ m1 (List l) es
        ParArray l es                 -> map (ParArray l) (listMuts es) ++ m1 (ParArray l) es
        Paren l e                     -> m1 (Paren l) e
        LeftSection l o e             -> m2 (LeftSection l) o e
        RightSection l e o            -> m2 (RightSection l) e o
        RecConstr l n fus             -> m2 (RecConstr l) n fus
        RecUpdate l e fus             -> m2 (RecUpdate l) e fus
        
        -- What other mutations can we perform on Enum...?
        EnumFrom l e                  -> List l [e] : m1 (EnumFrom l) e
        EnumFromTo l e1 e2            -> EnumFromTo l e2 e1 : m2 (EnumFromTo l) e1 e2
        EnumFromThen l e1 e2          -> EnumFromThen l e2 e1 : m2 (EnumFromThen l) e1 e2
        EnumFromThenTo l e1 e2 e3     -> EnumFromThenTo l e3 e2 e1 : m3 (EnumFromThenTo l) e1 e2 e3
        ParArrayFromTo l e1 e2        -> ParArrayFromTo l e2 e1 : m2 (ParArrayFromTo l) e1 e2
        ParArrayFromThenTo l e1 e2 e3 -> ParArrayFromThenTo l e3 e2 e1 : m3 (ParArrayFromThenTo l) e1 e2 e3
        ListComp l e qs               -> m2 (ListComp l) e qs
        ParComp l e qss               -> [] -- m2 (ParComp l) e q
        ParArrayComp l e qss          -> [] -- m2 (ParArrayComp l) e q
        ExpTypeSig l e t              -> [e] -- e : m2 (ExpTypeSig l) e t
        Proc l p e                    -> m2 (Proc l) p e
        LeftArrApp l e1 e2            -> m2 (LeftArrApp l) e1 e2
        RightArrApp l e1 e2           -> m2 (RightArrApp l) e1 e2
        LeftArrHighApp l e1 e2        -> m2 (LeftArrHighApp l) e1 e2
        RightArrHighApp l e1 e2       -> m2 (RightArrHighApp l) e1 e2

        _ -> []

        where mInject l = Var l ( UnQual l ( Ident l "mutateInj" ))
              -- TODO: Add more cases for how to shuffle guards, cases, 
              --       and lists.
              guardMuts gs = [reverse gs, last gs : init gs]
              caseMuts as = [reverse as, last as : init as]
              listMuts xs = [reverse xs, last xs : init xs, tail xs, init xs,
                             [head xs]]

instance Mutable (QualStmt a) where
    mutate qualStmt = case qualStmt of
        QualStmt l stmt      -> m1 (QualStmt l) stmt
        ThenTrans l e        -> m1 (ThenTrans l) e
        ThenBy l e1 e2       -> m2 (ThenBy l) e1 e2
        GroupBy l e          -> m1 (GroupBy l) e
        GroupUsing l e       -> m1 (GroupUsing l) e
        GroupByUsing l e1 e2 -> m2 (GroupByUsing l) e1 e2

instance Mutable (FieldUpdate a) where
    mutate fieldUpdate = case fieldUpdate of
        FieldUpdate l qName exp -> m2 (FieldUpdate l) qName exp
        _ -> []

instance Mutable (IPName a) where
  mutate _ = []

instance Mutable (Alt a) where
  mutate (Alt l pat rhs binds) = m3 (Alt l) pat rhs binds

instance Mutable Boxed where
  mutate Boxed = [Unboxed]
  mutate Unboxed = [Boxed]

instance Mutable (QOp a) where
    mutate (QVarOp l qName) = m1 (QVarOp l) qName
    mutate _                = []

instance Mutable (QName a) where
    mutate (UnQual l name) = m1 (UnQual l) name
    mutate _               = []

instance Mutable (Literal a) where
    mutate (Int l i s) = mapStrings $ map (\x -> Int l x s) intMuts
        where intMuts = mutate i

    mutate (Frac l rat _) = mapLit (Frac l) rat (ratMuts rat)
      where ratMuts r = filter (/= r) [0, 1, r+1, r-1, r*(-1), r/2, r*2, r/10, r*10,
                           (r * 2)/2, (r/2)*2, 
                           fromIntegral $ truncate r,
                           fromIntegral $ floor r, 
                           fromIntegral $ ceiling r, 
                           fromIntegral $ round r]

    mutate (Char l char _) = mapLit (Char l) char (charMuts char)
      where constChars = ['x', 'a', '%', '\0', '\n']
            charMuts c = filter (/= c) $ (case c of
                                           '\NUL'     -> [succ c]
                                           '\1114111' -> [pred c]
                                           _ -> [succ c, pred c]
                                         ) ++ constChars

    mutate (String l str _) = mapLit (String l) str (strMuts str)
      where strMuts xs = filter (/= xs) ["", ' ':xs, tail xs, reverse xs, 
                                         init xs, xs ++ " "]

    -- TODO: Unboxed literals(?)

    mutate _ = []
-- Helper function for mutate on Literals
mapLit constr param xs = map (\x -> constr x (show x)) xs

mapStrings :: [Literal a] -> [Literal a]
mapStrings [] = []
mapStrings (Int l i s : xs) = Int l i (show i) : mapStrings xs

instance Mutable Integer where
    mutate n = [n + 1, n - 1, -n, 0, 1, (fromIntegral n) :: Integer]    

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


