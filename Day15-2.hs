{-# LANGUAGE ViewPatterns #-}

import Data.Char (ord)
import Data.Maybe (mapMaybe)
import Data.List (minimumBy)
import Data.Tuple (swap)
import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.Set as S

main = readFile "Input15.txt" >>= print . snd . M.findMax . (\xs -> solve xs (S.singleton (0, (0, 0))) $ M.singleton (0, 0) 0)
    . M.fromList . concat . zipWith (\a -> zipWith (\b -> (,) (a, b)) [0..]) [0..] . fmap (fmap $ subtract (ord '0') . ord) . lines

-- Dijkstra algorithm
solve xs poss ds | S.null poss = ds
solve xs (S.deleteFindMin -> ((val, (a, b)), poss)) ds = solve xs (poss <> poss') $ M.fromList (swap <$> S.toList poss') <> ds
    where
        poss' = S.fromList $ mapMaybe (\p -> do
            pval <- get xs p
            guard . maybe True (> pval + val) $ ds M.!? p
            return (pval + val, p)) [(a + 1, b), (a - 1, b), (a, b + 1), (a, b - 1)]

get xs (a, b)
    | a >= x * 5 || b >= y * 5 || a < 0 || b < 0 = Nothing
    | otherwise = Just . succ . (`mod` 9) . pred . (a `div` x + b `div` y +) $ xs M.! (a `mod` x, b `mod` y)
    where (succ -> x, succ -> y) = fst $ M.findMax xs
