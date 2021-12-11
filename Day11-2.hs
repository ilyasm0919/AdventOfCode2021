import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.List (findIndex)
import Control.Monad (msum)

main = readFile "Input11.txt" >>= print . findIndex (all $ all (== 0)) . iterate (fmap (fmap $ fromMaybe 0) . solve . fmap (fmap $ Just . succ)) . fmap (fmap $ subtract (ord '0') . ord) . lines

solve xs | Just next <- step = solve next
    where
        step = (\(a, b) -> zipWith (\x -> zipWith (\y val -> if (a, b) == (x, y) then Nothing else if abs (a-x) <= 1 && abs (b-y) <= 1 then succ <$> val else val) [0..]) [0..] xs) <$>
            msum (zipWith (fmap . (,)) [0..] $ findIndex (maybe False (> 9)) <$> xs)
solve xs = xs
