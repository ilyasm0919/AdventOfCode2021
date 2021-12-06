import Data.Char (ord)

population = [1] ++ replicate 7 2 ++ replicate 1 3 ++ zipWith (+) population (drop 2 population)

main = readFile "Input6.txt" >>= print . sum . fmap ((!!) population . (-) 256) . filter (>= 0) . fmap (subtract (ord '0') . ord)
