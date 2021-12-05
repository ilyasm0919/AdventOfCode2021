main = readFile "Input2.txt" >>= print . (\(x, y, z) -> x*y) . foldl command (0, 0, 0) . fmap words . lines

command (a, b, c) ["forward", x] = (a+read x, b+c*read x, c)
command (a, b, c) ["down", x] = (a, b, c+read x)
command (a, b, c) ["up", x] = (a, b, c-read x)
