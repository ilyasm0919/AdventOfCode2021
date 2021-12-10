import Data.Maybe (fromJust, mapMaybe)
import Data.List (sort, elemIndex)

main = readFile "Input10.txt" >>= print . (\xs -> xs !! (length xs `div` 2)) . sort . mapMaybe (solve []) . lines

solve xs "" = Just $ foldl (\a x -> a*5 + 1 + fromJust (elemIndex x "([{<")) 0 xs
solve xs (y : ys) | y `elem` "([{<" = solve (y : xs) ys
solve (x : xs) (y : ys) | (x, y) `elem` zip "([{<" ")]}>" = solve xs ys
solve _ (y : _) = Nothing
