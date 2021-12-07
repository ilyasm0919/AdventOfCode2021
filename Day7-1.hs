import Data.List (sort)

main = readFile "Input7.txt" >>= print . solve . sort . fmap read . split

split "\n" = [""]
split (',' : xs) = "" : split xs
split (c : xs) = (c : y) : ys
    where y : ys = split xs

solve xs = sum $ fmap (abs . subtract m) xs
    where m = xs !! (length xs `div` 2)
