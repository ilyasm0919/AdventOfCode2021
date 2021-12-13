import Data.List (stripPrefix, sort, transpose)
import Data.Foldable (traverse_)
import Data.Bifunctor

main = readFile "Input13.txt" >>= traverse_ putStrLn . foldr printCode [""] . (\xs -> zipWith (\(x1, y1) (x2, y2) -> (x1 - x2, y1 - y2)) xs $ bimap minimum minimum (unzip xs) : xs)
    . sort . fmap (\(x, y) -> (y, x)) . uncurry (foldl $ flip solve) . bimap (fmap $ bimap read (read . tail) . break (== ',')) (fmap parseFold . tail) . break null . lines

parseFold str
    | Just val <- stripPrefix "fold along x=" str = Left $ read val
    | Just val <- stripPrefix "fold along y=" str = Right $ read val

solve (Left y) = fmap . first $ \x -> if x > y then 2 * y - x else x
solve (Right y) = fmap . second $ \x -> if x > y then 2 * y - x else x

printCode (0, 0) xss = xss
printCode (0, dy) (xs : xss) = ('#' : replicate (dy - 1) ' ' ++ xs) : xss
printCode (dx, dy) (xs : xss) = ('#' : replicate (length xs + dy) ' ') : replicate (dx - 1) "" ++ xs : xss
