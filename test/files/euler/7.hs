
-- Gets nth prime (yay lazy evaluation!)
getPrime :: Integer -> Integer
getPrime n = sieve [2..] n
    where sieve (p:ms) 1 = p
          sieve (p:ms) k = sieve (filter (\m -> m `mod` p /= 0) ms) (k-1)

main :: IO ()
main = print $ getPrime 10001
