import Control.Monad (replicateM)
import Control.Monad.State
import Data.Bifunctor (bimap)
import Data.Char (digitToInt)

main = readFile "Input16.txt" >>= print . snd . evalState solve . concatMap (bits 4 [] . digitToInt)

bits 0 xs x = xs
bits n xs x = bits (n - 1) (x `mod` 2 : xs) $ x `div` 2

solve = do
    v <- fromBits <$> get 3
    t <- fromBits <$> get 3
    if t == 4
        then bimap (+6) fromBits <$> t4
        else let op = [sum, product, minimum, maximum, undefined, bin (>), bin (<), bin (==)] !! t in
            get 1 >>= \[i] -> if i == 1
            then do
                l <- fromBits <$> get 11
                bimap ((+18) . sum) op . unzip <$> replicateM l solve
            else do
                l <- fromBits <$> get 15
                bimap (+22) op <$> i0 l
    where
        fromBits = foldl (\acc x -> x + acc * 2) 0
        get :: Int -> State [Int] [Int]
        get k = state $ splitAt k
        t4 = get 5 >>= \(b : bs) -> if b == 1 then bimap (+5) (bs ++) <$> t4 else return (5, bs)
        bin f [x, y] = if f x y then 1 else 0
        i0 l = solve >>= \(n, ans) -> if n >= l then return (n, [ans]) else bimap (+n) (ans:) <$> i0 (l - n)
