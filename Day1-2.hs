solve :: [Int] -> Int
solve xs = length . filter id . zipWith (<) xs $ tail xs

windows xs = zipWith (+) xs . tail  . zipWith (+) xs $ tail xs

main = readFile "Input1.txt" >>= print . solve . windows . fmap read . lines
