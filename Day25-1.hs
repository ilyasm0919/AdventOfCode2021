import Data.Maybe (fromMaybe)
import qualified Data.Set as S

main = do
    ls <- lines <$> readFile "Input25.txt"
    let h = length ls
        w = length $ head ls
        ls' = concat $ zipWith (\i -> zipWith (\j x -> ((i, j), x)) [0..]) [0..] ls
        east = S.fromList . fmap fst $ filter ((== '>') . snd) ls'
        south = S.fromList . fmap fst $ filter ((== 'v') . snd) ls'
        ans = solve 0 h w east south
    print ans

solve n h w east south
    | Nothing <- east', Nothing <- south' = succ n
    | otherwise = n `seq` solve (succ n) h w east'' south''
    where
        east' = move (\(x, y) -> (x, succ y `mod` w)) east south
        east'' = fromMaybe east east'
        south' = move (\(x, y) -> (succ x `mod` h, y)) south east''
        south'' = fromMaybe south south'

move f xs ys
    | null moved = Nothing
    | otherwise = Just $ S.map f moved <> notMoved
    where (notMoved, moved) = S.partition (\x -> f x `S.member` xs || f x `S.member` ys) xs
