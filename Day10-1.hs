import Data.Maybe (fromJust)

main = readFile "Input10.txt" >>= print . sum . fmap (solve []) . lines

solve xs "" = 0
solve xs (y : ys) | y `elem` "([{<" = solve (y : xs) ys
solve (x : xs) (y : ys) | (x, y) `elem` zip "([{<" ")]}>" = solve xs ys
solve _ (y : _) = fromJust $ lookup y $ zip ")}]>" [3, 57, 1197, 25137]
