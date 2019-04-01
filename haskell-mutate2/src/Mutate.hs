module Mutate
    ( mutate
    , safeMutate
    ) where

import GHC.Float
import Language.Haskell.Exts
import Data.List

-- | Defining a class Mutable. The function mutate takes a member of the 
--   class as an argument, and the result is of the same type. (?)
class Mutable a where
    mutate :: a -> [a]

safeMutate :: (Mutable a, Eq a) => a -> [a]
safeMutate a = filter (/= a) $ nub $ mutate a

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

instance Mutable (ModuleHead a) where
    mutate rest = []

instance Mutable (ModulePragma a) where
    mutate rest = []

instance Mutable (ImportDecl a) where
  mutate rest = []

instance Mutable (Decl a) where
    mutate decl = case decl of
        PatBind l pat rhs mbyBinds -> m3 (PatBind l) pat rhs mbyBinds

        _ -> []

instance Mutable (Rhs a) where
    mutate (UnGuardedRhs l exp) = m1 (UnGuardedRhs l) exp

    mutate (GuardedRhss l guardedRhss) = m1 (GuardedRhss l) guardedRhss

    mutate _ = []

instance Mutable (GuardedRhs a) where
    mutate (GuardedRhs l stmts exp) = m2 (GuardedRhs l) stmts exp

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
  mutate (App l e1 e2)                   = (App l (mInject l) (App l e1 e2)) : m2 (App l) e1 e2
    where mInject l = Var l ( UnQual l ( Ident l "mutateInj" ))
  mutate (Var l qn)                      = m1 (Var l) qn
  mutate (OverloadedLabel l str)         = []
  mutate (IPVar l n)                     = m1 (IPVar l) n
  mutate (Con l n)                       = m1 (Con l) n
  mutate (Lit l literal)                 = m1 (Lit l) literal
  mutate (InfixApp l e1 qOp e2)          = m3 (InfixApp l) e1 qOp e2
  mutate (NegApp l e)                    = e : mutate e ++ m1 (NegApp l) e
  mutate (Lambda l p e)                  = m2 (Lambda l) p e
  mutate (Let l b e)                     = m2 (Let l) b e
  mutate (If l ifExp thenExp elseExp)    = m3 (If l) ifExp thenExp elseExp
  mutate (MultiIf l g)                   = m1 (MultiIf l) g
  mutate (Case l e a)                    = m2 (Case l) e a
  mutate (Do l s)                        = m1 (Do l) s -- The last statement in the list should be an expression.
  mutate (MDo l s)                       = m1 (MDo l) s
  mutate (Tuple l b e)                   = m2 (Tuple l) b e
  mutate (UnboxedSum l i1 i2 e)          = [] -- m3 (UnboxedSum l) i1 i2 e
  mutate (TupleSection l b e)            = m2 (TupleSection l) b e
  mutate (List l e)                      = m1 (List l) e
  mutate (ParArray l e)                  = m1 (ParArray l) e
  mutate (Paren l e)                     = m1 (Paren l) e
  mutate (LeftSection l o e)             = m2 (LeftSection l) o e
  mutate (RightSection l e o)            = m2 (RightSection l) e o
  mutate (RecConstr l n u)               = [] -- m2 (RecConstr l) n u
  mutate (RecUpdate l e u)               = [] -- m2 (RecUpdate l) e u
  mutate (EnumFrom l e)                  = List l [e] : m1 (EnumFrom l) e
  mutate (EnumFromTo l e1 e2)            = m2 (EnumFromTo l) e1 e2
  mutate (EnumFromThen l e1 e2)          = m2 (EnumFromThen l) e1 e2
  mutate (EnumFromThenTo l e1 e2 e3)     = m3 (EnumFromThenTo l) e1 e2 e3
  mutate (ParArrayFromTo l e1 e2)        = m2 (ParArrayFromTo l) e1 e2
  mutate (ParArrayFromThenTo l e1 e2 e3) = m3 (ParArrayFromThenTo l) e1 e2 e3
  mutate (ListComp l e q)                = [] -- m2 (ListComp l) e q
  mutate (ParComp l e q)                 = [] -- m2 (ParComp l) e q
  mutate (ParArrayComp l e q)            = [] -- m2 (ParArrayComp l) e q
  mutate (ExpTypeSig l e t)              = [e] -- e : m2 (ExpTypeSig l) e t
  mutate (Proc l p e)                    = m2 (Proc l) p e
  mutate (LeftArrApp l e1 e2)            = m2 (LeftArrApp l) e1 e2
  mutate (RightArrApp l e1 e2)           = m2 (RightArrApp l) e1 e2
  mutate (LeftArrHighApp l e1 e2)        = m2 (LeftArrHighApp l) e1 e2
  mutate (RightArrHighApp l e1 e2)       = m2 (RightArrHighApp l) e1 e2
  mutate _                               = []


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
    mutate (Int l int _)  = mapLit (Int l) int (intMuts int)
      where intMuts n = filter (/= n) [0, 1, n+1, n-1, n*(-1)]

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
                                           minBound -> [succ c]
                                           maxBound -> [pred c]
                                           _ -> [succ c, pred c]
                                         ) ++ constChars

    mutate (String l str _) = mapLit (String l) str (strMuts str)
      where strMuts xs = filter (/= xs) ["", ' ':xs, tail xs, reverse xs, init xs, xs ++ " "]

    -- TODO: Unboxed literals(?)

    mutate _ = []
-- Helper function for mutate on Literals
mapLit constr param xs = map (\x -> constr x (show x)) xs

instance (Mutable a) => Mutable (Maybe a) where
    mutate (Just a) = Nothing : m1 Just a
    mutate _        = []

instance (Mutable a) => Mutable [a] where
    mutate []     = []
    mutate (x:xs) = m2 (:) x xs


