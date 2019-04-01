module MutateInject where

import Data.List

class Injectable a where
    mutateInj :: a -> a

-- TODO: Specific behaviour depending on type of a?
instance Injectable [a] where
    mutateInj xs = pickOne mutants
        where mutants = [reverse xs, tail xs, init xs]

instance Injectable Int where
    mutateInj n = pickOne mutants
        where mutants = [n + 1, n - 1, -n, 0, 1, maxBound, minBound,
                         n + maxBound, n - maxBound]

instance Injectable Integer where
    mutateInj n = pickOne mutants
        where mutants = [n + 1, n - 1, -n, 0, 1, (fromIntegral n) :: Integer]

instance Injectable Double where
    mutateInj r = pickOne mutants
        where mutants = [0, 1, r+1, r-1, r*(-1), r/2, r*2, r/10, r*10,
                        (r * 2)/2, (r/2)*2, 
                        fromIntegral $ truncate r,
                        fromIntegral $ floor r, 
                        fromIntegral $ ceiling r, 
                        fromIntegral $ round r]

instance Injectable Float where
    mutateInj r = pickOne mutants
        where mutants = [0, 1, r+1, r-1, r*(-1), r/2, r*2, r/10, r*10,
                        (r * 2)/2, (r/2)*2, 
                        fromIntegral $ truncate r,
                        fromIntegral $ floor r, 
                        fromIntegral $ ceiling r, 
                        fromIntegral $ round r]

instance Injectable Char where
    mutateInj c = pickOne mutants
        where mutants = constChars ++ charMuts c
              constChars = ['x', 'a', '%', '\0', '\n']
              charMuts c = filter (/= c) $ case c of
                                            minBound -> [succ c]
                                            maxBound -> [pred c]
                                            _ -> [succ c, pred c]

instance Injectable (Maybe a) where
    mutateInj m = pickOne mutants
        where mutants = case m of
                            Just a -> [Nothing]
        

{- TODO: How to do (a, a)?
instance Injectable (a, a) where
    mutateInj (x, y) = pickOne mutants
        where mutants = [(y, x)]
-}                  

-- TODO
pickOne :: [a] -> a
pickOne (x:xs) = x
pickOne _      = error "pickOne on empty list"




