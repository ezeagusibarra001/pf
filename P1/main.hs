
doble :: Int -> Int
doble x = x + x

cuadruple :: Int -> Int
cuadruple y = 4 * y

twice :: (Int -> Int) -> (Int -> Int)
twice f = g
    where g x = f (f x)