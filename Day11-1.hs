import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.List (findIndex)
import Control.Monad (msum)

main = readFile "Input11.txt" >>= print . fst . foldr (.) id (replicate 100 $ fmap (fmap . fmap $ fromMaybe 0) . solve . fmap (fmap . fmap $ Just . succ)) . (,) 0 . fmap (fmap $ subtract (ord '0') . ord) . lines

solve (n, xs) | Just next <- step = solve $ n `seq` (n + 1, next)
    where
        step = (\(a, b) -> zipWith (\x -> zipWith (\y val -> if (a, b) == (x, y) then Nothing else if abs (a-x) <= 1 && abs (b-y) <= 1 then succ <$> val else val) [0..]) [0..] xs) <$>
            msum (zipWith (fmap . (,)) [0..] $ findIndex (maybe False (> 9)) <$> xs)
solve (n, xs) = (n, xs)
