import Control.Applicative (liftA2)

main = readFile "Input20.txt" >>= print . length . filter (== 1) . concat . snd . (liftA2 (.) solve solve <$> head <*> (,) 0 . drop 2) . fmap (fmap $ \c -> if c == '#' then 1 else 0) . lines

solve table (inf, xs) = (inf', xs')
    where
        inf' | inf == 0 = head table
             | inf == 1 = last table
        xs' = extendWith (zipWith3 $ \x y z -> (table !!) . sum . zipWith (*) (iterate (*2) 1) . reverse $ concat [x, y, z]) (repeat [inf, inf, inf]) (extendWith (\x y z -> [x, y, z]) inf <$> xs)

extendWith f x xs = let xs' = xs ++ [x, x] in zipWith3 f (x : x : xs') (x : xs') xs'
