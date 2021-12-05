solve :: [Int] -> Int
solve xs = length . filter id . zipWith (<) xs $ tail xs

main = readFile "Input1.txt" >>= print . solve . fmap read . lines
