import Data.List (transpose)
import Data.Char (ord)

main = readFile "Input9.txt" >>= print . sum . fmap succ . solve . fmap (fmap $ subtract (ord '0') . ord) . lines

solve xs = fmap fst . filter (\(a, b) -> a < b) . concat . zipWith zip xs . fmap (fmap minimum . transpose) $ transpose [tail xs, []:init xs, fmap tail xs, fmap ((10:) . init) xs]
