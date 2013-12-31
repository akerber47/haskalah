-- Can get Pythagorean triplets via 2ab, a^2-b^2,a^2+b^2 trick

-- Brute-force some values of a and b

main :: IO ()
main = print answer
    where possibleTrips :: [(Int,Int)]
          possibleTrips = [(a,b) | a <- [1..500], b <- [1..500]]
          passedTrips = filter (\(a,b) -> a>b && (2*a*b)+(a*a-b*b)+(a*a+b*b) == 1000) possibleTrips
          x = fst $ head passedTrips
          y = snd $ head passedTrips
          answer = (2*x*y)*(x*x-y*y)*(x*x+y*y)
