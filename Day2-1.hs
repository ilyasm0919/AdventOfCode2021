import Data.Monoid (Sum(..))

main = readFile "Input2.txt" >>= print . (\(x, y) -> getSum x*getSum y) . foldMap (command . words) . lines

command ["forward", x] = (Sum $ read x, 0)
command ["down", x] = (0, Sum $ read x)
command ["up", x] = (0, Sum $ -read x)
