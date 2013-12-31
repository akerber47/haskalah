-- Project Euler: Problem 1

nums :: [Integer]
nums = 1:map (+1) nums
natmult :: [Integer]
natmult = filter (\x -> (mod x 3) == 0 || (mod x 5) == 0) nums
natless :: [Integer]
natless = filter (< 1000) (take 1000 natmult)

main :: IO ()
main = print $ sum natless
