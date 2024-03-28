{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}


first :: (a, b) -> a
first (x,y) = x

apply :: (t1 -> t2) -> t1 -> t2
apply f = g
    where g x = f x