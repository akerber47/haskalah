fib :: (Ix a, Num b) => a -> b
fib n = fibs !! n
where fibs = 0:1:(zipWith (+) fibs (tail fibs))
