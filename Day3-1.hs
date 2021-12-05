import Data.List (transpose)

solve xs = length (filter id xs) > length (filter not xs)

decimal = sum . fmap fst . filter snd . zip (iterate (*2) 1) . reverse

main = readFile "Input3.txt" >>= print . (\x -> decimal x * decimal (not <$> x)) . fmap solve . transpose . fmap (fmap (== '1')) . lines
