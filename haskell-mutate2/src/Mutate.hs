module Mutate
    ( mutate
    ) where

import GHC.Float
import Language.Haskell.Exts
import Debug.Trace

-- | Defining a class Mutable. The function mutate takes a member of the 
-- class as an argument, and the result is of the same type. (?)
class Mutable a where
    mutate :: a -> [a]

--
m1 :: (Mutable a) => (a -> b) -> a -> [b]
m1 f a = aMutants
    where aMutant = trace ("MUTATING M1: ") $ mutate a
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
          bMutant = trace "THIS IS THE B MUTANT" (mutate b)
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
    mutate (Module l mbyHead pragmas importDecls decls) =
        trace ("MUTATED M4: \n" ++ unlines (map prettyPrint (m4 (Module l) mbyHead pragmas importDecls decls))) $ m4 (Module l) mbyHead pragmas importDecls decls

instance Mutable (ModuleHead a) where
    mutate rest = [rest]

instance Mutable (ModulePragma a) where
    mutate rest = [rest]

instance Mutable (ImportDecl a) where
    mutate rest = [rest]

instance Mutable (Decl a) where
    mutate decl = case decl of
        FunBind l matches -> m1 (FunBind l) matches

        _ -> [decl]

instance Mutable (Rhs a) where
    mutate rhs = case rhs of
        UnGuardedRhs l exp -> m1 (UnGuardedRhs l) exp

        _ -> [rhs]

instance Mutable (Match a) where
    mutate match = case match of
        Match l name pat rhs mbyBinds ->
            m4 (Match l) name pat rhs mbyBinds
        _ -> [match]

instance Mutable (Name a) where
    mutate name = case name of
        Symbol l s -> [(Symbol l "-")]

        _ -> [name]

instance Mutable (Pat a) where
    mutate pat = case pat of
        _ -> [pat]

instance Mutable (Exp a) where
    mutate (InfixApp l e1 qOp e2)       = trace "INFIXAPP" $ m3 (InfixApp l) e1 qOp e2
    mutate (If l ifExp thenExp elseExp) = m3 (If l) ifExp thenExp elseExp
    mutate rest = [rest]

instance Mutable (QOp a) where
    mutate (QVarOp l qName) = m1 (QVarOp l) qName
    mutate rest             = [rest]

instance Mutable (QName a) where
    mutate (UnQual l name) = m1 (UnQual l) name
    mutate rest = [rest]

instance Mutable (Maybe a) where
    mutate rest = [rest]

instance (Mutable a) => Mutable [a] where
    mutate xs = map mutate xs
