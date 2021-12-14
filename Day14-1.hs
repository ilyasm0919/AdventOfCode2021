import Data.List (sort, group)
import Data.Maybe (maybeToList)

main = readFile "Input14.txt" >>= print . (\xs -> maximum xs - minimum xs) . fmap length . group . sort . uncurry (foldr (.) id . replicate 10 . solve)
    . (\(x : "" : xs) -> (fmap (\(y : z : end) -> ((y, z), last end)) xs, x)) . lines

solve xs ys = (head ys:) . concat . zipWith (\x y -> maybeToList (lookup (x, y) xs) ++ [y]) ys $ tail ys
