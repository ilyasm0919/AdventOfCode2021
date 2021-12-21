main = readFile "Input21.txt" >>= print . solve 0 0 0 . fmap (read . pure . last) . lines

solve rots score1 score2 [pos1, pos2]
    | score1 >= 1000 = rots * score2
    | score2 >= 1000 = rots * score1
    | otherwise = solve (rots + 3) score2 (score1 + pos') [pos2, pos']
    where pos' = succ . (`mod` 10) . (pos1+2+) . sum . fmap (`mod` 100) . take 3 $ iterate succ rots
