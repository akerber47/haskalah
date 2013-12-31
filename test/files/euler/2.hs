fibsumHelper :: (Integral a) => a -> a -> a -> a
fibsumHelper 0 _ _ = 0
fibsumHelper 1 _ _ = 1
fibsumHelper n i j = if i <= n
                      then if (even i)
                              then i+(fibsumHelper n (i+j) i)
                              else fibsumHelper n (i+j) i
                      else 0
fibsum :: Integer -> Integer
fibsum n = fibsumHelper n 1 1

main :: IO ()
main = print $ fibsum 4000000
