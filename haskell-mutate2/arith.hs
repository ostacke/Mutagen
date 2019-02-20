{- Performs computationally intensive additions, so that we do not need to -}

add :: Integer -> Integer -> Integer
add x y = x + y

filterEven :: [Integer] -> [Integer]
filterEven [] = []
filterEven (x:xs) | even x    = x : filterEven xs
                  | otherwise = filterEven xs

double :: (Num a) => a -> a
double n = n * 2

