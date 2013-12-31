import Data.List

-- Same factorization algorithm as from #3
-- Kind of stupid to use it here as it's totally stateless and will sieve
-- repeatedly (herp derp).

-- Pick out which primes divide the given integer.
-- Helper uses accfactors for tail recursion.
-- Takes in number to factor, list of things that are possibly primes (to check
-- against), and factor accumulator list.
-- For efficiency we only sieve the possible prime list as we need to.
factors :: Int -> [Int]
factors n =  factorHelper n [2..n] []
    where sieve :: Int -> [Int] -> [Int]
          sieve p ps = filter (\m -> m `mod` p /= 0) ps
          factorHelper :: Int -> [Int] -> [Int] -> [Int]
          factorHelper 1 _      accfactors = accfactors
          factorHelper m []     accfactors = (m:accfactors)
          factorHelper m (p:ps) accfactors | m `mod` p == 0 = factorHelper (m `div` p) (p:ps) (p:accfactors)
                                           | otherwise      = factorHelper m (sieve p ps) accfactors

-- Counts divisors of a number (standard trick w/ prime factorization)
countDivs :: Int -> (Int, Int)
countDivs n = (n,product $ map (+1) facCounts)
    where facs      = factors n
          facCounts = map (\i -> length $ filter (== i) facs) $ nub facs

main :: IO ()
main = print $ fst $ head $ dropWhile ((< 500) . snd) triDivs
    where triDivs = map (countDivs . (\n -> n*(n+1)`div`2)) $ [5..]
