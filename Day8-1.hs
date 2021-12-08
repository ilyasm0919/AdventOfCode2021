main = readFile "Input8.txt" >>= print . length . filter (flip elem [2, 3, 4, 7] . length) . concat . fmap (drop 11 . words) . lines
