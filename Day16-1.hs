import Control.Monad (replicateM)
import Control.Monad.Writer
import Control.Monad.State
import Data.Char (digitToInt)
import Data.Monoid (Sum(..))

main = readFile "Input16.txt" >>= print . getSum . snd . runWriter . evalStateT solve . concatMap (bits 4 [] . digitToInt)

bits 0 xs x = xs
bits n xs x = bits (n - 1) (x `mod` 2 : xs) $ x `div` 2

solve = do
    v <- fromBits <$> get 3
    tell $ Sum v
    t <- fromBits <$> get 3
    if t == 4
        then fmap (+6) t4
        else get 1 >>= \[i] -> if i == 1
            then do
                l <- fromBits <$> get 11
                (+18) . sum <$> replicateM l solve
            else do
                l <- fromBits <$> get 15
                (+22) <$> i0 l
    where
        fromBits = foldl (\acc x -> x + acc * 2) 0
        get :: Int -> StateT [Int] (Writer (Sum Int)) [Int]
        get k = state $ splitAt k
        t4 = get 5 >>= \(b : bs) -> if b == 1 then fmap (+5) t4 else return 5
        i0 l = solve >>= \n -> if n >= l then return n else (+n) <$> i0 (l - n)
