import Control.Applicative (ZipList(..))
import Control.Monad (replicateM)
import Data.List (sort, group)

main = readFile "Input21.txt" >>= print . solve . fmap (read . pure . last) . lines

solve [pos1, pos2] = max (sum $ zipWith (*) wins1 (1 : notWins2)) (sum $ zipWith (*) wins2 notWins1)
    where
        wins1 = take 22 $ wins 0 pos1
        wins2 = take 22 $ wins 0 pos2
        notWins1 = take 22 $ notWins 0 pos1
        notWins2 = take 22 $ notWins 0 pos2

wins score pos
    | score >= 21 = 1 : repeat 0
    | otherwise = 0 : fmap sum (getZipList $ traverse (\xs -> let pos' = succ $ (pred $ pos + head xs) `mod` 10 in
        ZipList . fmap (* length xs) $ wins (score + pos') pos') . group . sort . fmap sum $ replicateM 3 [1, 2, 3])

notWins score pos
    | score >= 21 = repeat 0
    | otherwise = 1 : fmap sum (getZipList $ traverse (\xs -> let pos' = succ $ (pred $ pos + head xs) `mod` 10 in
        ZipList . fmap (* length xs) $ notWins (score + pos') pos') . group . sort . fmap sum $ replicateM 3 [1, 2, 3])
