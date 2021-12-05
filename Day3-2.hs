import Data.List (transpose)

solve f = fst . step . fmap (\x -> (x, x))
    where
        step [x] = x
        step xs = step $ fmap tail <$> filter ((bit ==) . head . snd) xs
            where bit = f $ length (filter (head . snd) xs) >= length (filter (not . head . snd) xs)

decimal = sum . fmap fst . filter snd . zip (iterate (*2) 1) . reverse

main = readFile "Input3.txt" >>= print . (\x -> decimal (solve id x) * decimal (solve not x)) . fmap (fmap (== '1')) . lines
