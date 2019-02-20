{- Performs computationally intensive additions, so that we do not need to -}

add :: Integer -> Integer -> Integer
add x y = x + y

double :: (Num a) => a -> a
double n = n * 2

