fac :: (Num a) => a -> a
fac 0 = 1
fac n = n * fac (n-1)
