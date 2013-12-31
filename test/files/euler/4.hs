-- Number assumed to have 6 digits
isPal :: Int -> Bool
isPal n = d1 == d6 && d2 == d5 && d4 == d3
    where d1 = n `mod` 10
          d2 = n `mod` 100 `div` 10
          d3 = n `mod` 1000 `div` 100
          d4 = n `mod` 10000 `div` 1000
          d5 = n `mod` 100000 `div` 10000
          d6 = n `div` 100000

-- List of all possible digits of 2 3-digit #s
possiblePals :: [Int]
possiblePals = [a*b | a <- [100..999], b <- [100..999]]
                            
main :: IO ()
main = print $ maximum $ filter isPal possiblePals
