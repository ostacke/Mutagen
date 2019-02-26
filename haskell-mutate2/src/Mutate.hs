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

instance Mutable Exp a where
    mutate (InfixApp l e1 op e2) = m3 (InfixApp l) e1 op e2

instance Mutable (Name a) where
    mutate (Symbol l str) = case str of
        "*" -> Symbol l "+"
        _   -> Symbol l str
    mutate x = x

instance Mutable Bool where
    mutate = not

instance Mutable Char where
    mutate = succ
    -- mutate c = pred c

instance Mutable a => Mutable (a, b) where
    mutate (x, y) = (mutate x, y)
    -- Recursive calls to mutate on x and y?
    -- How to behave with tuples of larger sizes?
    -- is there a way to "shuffle them around"?

instance Mutable () where
    mutate () = ()

instance Mutable Integer where
    mutate 0 = 1
    mutate 1 = 0
    mutate x = x + 1
    -- mutate x = x - 1

instance Mutable Int where
    mutate 0 = 1
    mutate 1 = 0
    mutate x = x + 1
    -- mutate x = x - 1

instance Mutable Float where
    mutate = double2Float . float2Double
    -- mutate x = fromInteger (ceiling x)
    -- mutate x = fromInteger (truncate x)
    -- mutate x = fromInteger (round x)

instance Mutable Double where
    mutate = float2Double . double2Float
    -- mutate x = fromInteger (ceiling x)
    -- mutate x = fromInteger (truncate x)
    -- mutate x = fromInteger (round x)

instance Mutable [a] where
    mutate (x:xs) = xs
    mutate _ = []
    -- mutate (x:xs) = x : (x:xs)
    -- mutate (x:xs) = x : reverse xs

