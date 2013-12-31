-- Um just use sum formulas
ans :: Integer
ans = ((100*101) `div` 2)*((100*101) `div` 2) - ((100*101*201) `div` 6)

main :: IO ()
main = print ans
