
-- Sums primes up to nth (yay lazy evaluation!)
sumPrimesTo :: Integer -> Integer
sumPrimesTo n = sieve [2..] 0
    where sieve (p:ms) sumacc | p >= n = sumacc
                              | p < n  = sieve (filter (\m -> m `mod` p /= 0) ms) (p+sumacc)

main :: IO ()
main = print $ sumPrimesTo 2000000
