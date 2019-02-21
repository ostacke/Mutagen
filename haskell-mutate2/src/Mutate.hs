module Mutate
( mutate
) where
    
import GHC.Float

-- | Defining a class, "Mutable". The function mutate takes a member of the 
-- class as an argument, and the result is of the same type. (?)
class Mutable a where
    mutate :: a -> a

-- Other suggestions for mutations in comments

instance Mutable Bool where
    mutate True = False
    mutate False = True

instance Mutable Char where
    mutate c = succ c
    -- mutate c = pred c

instance Mutable (a, b) where
    mutate (x, y) = (x, y)
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
    mutate x = double2Float (float2Double x)
    -- mutate x = fromInteger (ceiling x)
    -- mutate x = fromInteger (truncate x)
    -- mutate x = fromInteger (round x)

instance Mutable Double where
    mutate x = float2Double (double2Float x)
    -- mutate x = fromInteger (ceiling x)
    -- mutate x = fromInteger (truncate x)
    -- mutate x = fromInteger (round x)

instance Mutable [a] where
    mutate (x:xs) = xs
    -- mutate (x:xs) = x:(x:xs)
    -- mutate (x:xs) = x : reverse xs

    