import Data.List (sortOn, find)
import Data.Char (ord)
import Data.Maybe (isNothing)
import Data.Ord (Down(..))

main = readFile "Input9.txt" >>= print . product . take 3 . sortOn Down . solve . fmap (fmap $ subtract (ord '0') . ord) . lines

solve xs = fmap fst . filter (isNothing . snd) $ concat zs
    where
        n = length xs - 1
        m = length (head xs) - 1
        ys :: [[((Int, Int), [(Int, Int)])]]
        ys = fmap (\a -> fmap (\b -> ((a, b), filter (\(x, y) -> x >= 0 && x <= n && y >= 0 && y <= m) [(a-1, b), (a+1, b), (a, b-1), (a, b+1)])) [0..m]) [0..n]
        zs :: [[({- Sum of neighbour`s sizes -} Int, {- One of neighbour -} Maybe (Int, Int))]]
        zs = fmap (fmap $ \((a, b), ns) ->
            let val = xs !! a !! b
                zs' = fmap (\(x, y) -> zs !! x !! y) ns in
            ( if val == 9 then 0 else succ . sum . fmap fst $ filter ((== Just (a, b)) . snd) zs'
            , find (\(x, y) -> xs !! x !! y < val) ns)) ys
