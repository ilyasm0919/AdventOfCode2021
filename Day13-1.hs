import Data.List (stripPrefix, nub)
import Data.Bifunctor

main = readFile "Input13.txt" >>= print . length . nub . uncurry (flip solve) . fmap head . bimap (fmap $ bimap read (read . tail) . break (== ',')) (fmap parseFold . tail) . break null . lines

parseFold str
    | Just val <- stripPrefix "fold along x=" str = Left $ read val
    | Just val <- stripPrefix "fold along y=" str = Right $ read val

solve (Left y) = fmap . first $ \x -> if x > y then 2 * y - x else x
solve (Right y) = fmap . second $ \x -> if x > y then 2 * y - x else x
