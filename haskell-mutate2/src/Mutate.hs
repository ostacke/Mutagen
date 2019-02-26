module Mutate
    ( mutate
    ) where
    
import GHC.Float
import Language.Haskell.Exts

-- | Defining a class, "Mutable". The function mutate takes a member of the 
-- class as an argument, and the result is of the same type. (?)
class Mutable a where
    mutate :: a -> [a]

-- a -> [a] eller ngn annan struktur
-- Other suggestions for mutations in comments

-- | Returns the list of mutants created by applying mutate on types
--   with two type parameters (excluding location l)
m2 :: (Mutable a, Mutable b) => (a -> b -> c) -> a -> b -> [c]
m2 f a b = aMutants ++ bMutants
    where aMutant = mutate a
          bMutant = mutate b
          aMutants = map (\x -> f x b) aMutant
          bMutants = map (\x -> f a x) bMutant

-- | Same as m2, but with three parameters
m3 :: (Mutable a, Mutable b, Mutable c) => (a -> b -> c -> d) -> a -> b -> c -> [d]
m3 f a b c = aMutants ++ bMutants ++ cMutants
    where aMutant = mutate a
          bMutant = mutate b
          cMutant = mutate c
          aMutants = map (\x -> f x b c) aMutant
          bMutants = map (\x -> f a x c) bMutant
          cMutants = map (\x -> f a b x)s cMutant

instance Mutable (Exp a) where
    mutate (InfixApp l e1 op e2) = m3 (InfixApp l) e1 op e2
    mutate rest = [rest]

instance Mutable (QOp a) where
    mutate rest = [rest]

instance (Mutable a) => Mutable [a] where
    mutate xs = map mutate xs 

