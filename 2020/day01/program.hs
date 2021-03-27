
numbers =   (lines <$> readFile "input")
f0 x = map read x :: [Int]

-- Find the two entries that sum to 2020; what do you get if you multiply them together?

f1 ns = head [ x*y | x <- ns, y <- ns, x<y, x+y == 2020]

solve1 x = f1 (f0 x)
solution1 = fmap solve1 numbers -- 1016619

-- what is the product of the three entries that sum to 2020?

f2 ns = head [ x*y*z | x <- ns, y <- ns, z <- ns, x<y, y<z, x+y+z == 2020]

solve2 x = f2 (f0 x)
solution2 = fmap solve2 numbers -- 218767230