import Control.Monad.State

newtype Field = Field { getField :: [[Int]] }

instance Semigroup Field where
    Field xs <> Field ys = Field $ longZip (longZip (+)) xs ys
        where
            longZip f [] ys = ys
            longZip f xs [] = xs
            longZip f (x : xs) (y : ys) = f x y : longZip f xs ys

instance Monoid Field where
    mempty = Field []

main = readFile "Input5.txt" >>= print . sum . dmap (length . filter (>= 2)) . getField . foldMap command . lines

command = evalState $ do
    x1 <- number
    modify $ drop 1
    y1 <- number
    modify $ drop 4
    x2 <- number
    modify $ drop 1
    y2 <- number
    return . Field $ if x1 == x2 then replicate x1 [] ++ [replicate (min y1 y2) 0 ++ replicate (abs (y2 - y1) + 1) 1]
                else if y1 == y2 then replicate (min x1 x2) [] ++ replicate (abs (x1 - x2) + 1) (replicate y1 0 ++ [1])
                else []
    where
        number = state $ head . reads
