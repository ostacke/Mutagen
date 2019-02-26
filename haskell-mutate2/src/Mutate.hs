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

-- mutate3 som tar funktion/konstruktor med flera konstruerare,
m3 :: (Mutable a, Mutable b, Mutable c) => (a -> b -> c -> d) -> a -> b -> c -> [d]
m3 f a b c = dsa ++ dsb ++ dsc
    where as = mutate a
          bs = mutate b
          cs = mutate c
          dsa = map (\x -> f x b c) as
          dsb = map (\x -> f a x c) bs
          dsc = map (\x -> f a b x) cs

instance Mutable (Exp a) where
    mutate (InfixApp l e1 op e2) = m3 (InfixApp l) e1 op e2
    mutate rest = [rest]

instance Mutable (QOp a) where
    mutate rest = [rest]

instance (Mutable a) => Mutable [a] where
    mutate xs = map mutate xs 

