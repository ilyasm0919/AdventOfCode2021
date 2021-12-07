main = readFile "Input7.txt" >>= print . solve . fmap read . split

split "\n" = [""]
split (',' : xs) = "" : split xs
split (c : xs) = (c : y) : ys
    where y : ys = split xs

solve xs = minimum $ fmap (\n -> sum $ fmap ((\x -> x*(x+1)`div`2) . abs . subtract n) xs) [m-1, m, m+1]
    where m = sum xs `div` length xs
    -- abs (x-m)*(abs (x-m)+1)) == x^2 - 2*m*x + m^2 + abs (x-m)
    -- Total sum: CONST - 2*sum*m + m^2*count + sum_x(abs (x-m))
    -- Minimum at: (2*sum+-count)/(2*count) == sum/count +- 0.5
