-- I can implement this using dijkstra algorithm (or another algorithm for shortest path in graph), but I want to try something exotic like this
import Data.Char (ord)

data N = Z | S N deriving Eq

instance Semigroup N where
    S n <> S m = S $ n <> m
    _ <> _ = Z

instance Monoid N where
    mempty = S mempty

addN :: Int -> N -> N
addN n = (!! n) . iterate S

toInt :: N -> Int
toInt Z = 0
toInt (S x) = succ $ toInt x

myZip :: ([b] -> b) -> (a -> b -> c) -> [a] -> [[b]] -> [c]
myZip f g [] yss = []
myZip f g (x : xs) yss = g x (f $ fmap head yss) : myZip f g xs (fmap tail yss)

main = readFile "Input15.txt" >>= print . toInt . last . last . init . solve Nothing . fmap (fmap $ subtract (ord '0') . ord) . lines

solve prev [] = [repeat mempty]
solve prev (xs : xss) = ys : yss
    where
        ys | Just prev' <- prev = myZip mconcat addN xs [prev', mempty : init ys, tail ys ++ [mempty], head yss]
           | otherwise = Z : myZip mconcat addN (tail xs) [init ys, tail (tail ys) ++ [mempty], tail $ head yss]
        yss = solve (Just ys) xss
