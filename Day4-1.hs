import Data.Maybe
import Data.List (transpose)

main = do
    ls <- lines <$> readFile "Input4.txt"
    let numbers = fmap read . split $ head ls
        fields = chuncks . fmap (fmap (Just . read) . words) . filter (not . null) $ tail ls
    print $ solve fields numbers

split "" = []
split (',' : xs) = "" : split xs
split (c : xs) = (c : y) : ys
    where y : ys = split xs

chuncks [] = []
chuncks xs = take 5 xs : chuncks (drop 5 xs)

solve fs (x : xs)
    | null fs'' = solve fs' xs
    | otherwise = (*x) . sum . catMaybes . concat $ head fs''
    where
        fs' = fmap (fmap $ \y -> if Just x == y then Nothing else y) <$> fs
        fs'' = filter win fs'
        win x = any (all isNothing) x || any (all isNothing) (transpose x)
