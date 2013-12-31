-- This algorithm sucks less than before

-- Pick out which primes divide the given integer.
-- Helper uses accfactors for tail recursion.
-- Takes in number to factor, list of things that are possibly primes (to check
-- against), and factor accumulator list.
-- For efficiency we only sieve the possible prime list as we need to.
factors :: Integer -> [Integer]
factors n =  factorHelper n [2..n] []
    where sieve :: Integer -> [Integer] -> [Integer]
          sieve p ps = filter (\m -> m `mod` p /= 0) ps
          factorHelper :: Integer -> [Integer] -> [Integer] -> [Integer]
          factorHelper 1 _      accfactors = accfactors
          factorHelper m []     accfactors = (m:accfactors)
          factorHelper m (p:ps) accfactors | m `mod` p == 0 = factorHelper (m `div` p) (p:ps) (p:accfactors)
                                           | otherwise      = factorHelper m (sieve p ps) accfactors

main :: IO ()
main = print $ maximum $ factors 600851475143
