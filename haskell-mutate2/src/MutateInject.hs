module MutateInject where

import Data.List

-- | mutateInj takes a "seed" and returns a pseudo-random mutation of the 
--   input parameter.
class Injectable a where
    mutateInj :: Int -> a -> a

-- TODO: Specific behaviour depending on type of a?
instance Injectable [a] where
    mutateInj n xs = pickOne n mutants
        where mutants = [reverse xs, tail xs, init xs]

instance Injectable Int where
    mutateInj n int = pickOne n mutants
        where mutants = [int + 1, int - 1, -int, 0, 1, maxBound, minBound,
                         int + maxBound, int - maxBound]

instance Injectable Integer where
    mutateInj n int = pickOne n mutants
        where mutants = [int + 1, int - 1, -int, 0, 1, (fromIntegral int) :: Integer]

instance Injectable Double where
    mutateInj n r = pickOne n mutants
        where mutants = [0, 1, r+1, r-1, r*(-1), r/2, r*2, r/10, r*10,
                        (r * 2)/2, (r/2)*2, 
                        fromIntegral $ truncate r,
                        fromIntegral $ floor r, 
                        fromIntegral $ ceiling r, 
                        fromIntegral $ round r]

instance Injectable Float where
    mutateInj n r = pickOne n mutants
        where mutants = [0, 1, r+1, r-1, r*(-1), r/2, r*2, r/10, r*10,
                        (r * 2)/2, (r/2)*2, 
                        fromIntegral $ truncate r,
                        fromIntegral $ floor r, 
                        fromIntegral $ ceiling r, 
                        fromIntegral $ round r]

instance Injectable Char where
    mutateInj n c = pickOne n mutants
        where mutants = constChars ++ charMuts c
              constChars = ['x', 'a', '%', '\0', '\n']
              charMuts c = filter (/= c) $ case c of
                                            '\NUL'     -> [succ c]
                                            '\1114111' -> [pred c]
                                            _ -> [succ c, pred c]

instance Injectable (Maybe a) where
    mutateInj n m = pickOne n mutants
        where mutants = case m of
                            Just a -> [Nothing]
        
pickOne :: Int -> [a] -> a
pickOne n xs = xs !! ((9999 * n) `mod` length xs)




