import Control.Monad.State
import qualified Data.Set as S

main = readFile "Input22.txt" >>= print . S.size . foldl solve mempty . fmap parse . lines

parse = evalState $ do
    onOrOff <- while (/= ' ')
    skip 3
    x1 <- number
    skip 2
    x2 <- number
    skip 3
    y1 <- number
    skip 2
    y2 <- number
    skip 3
    z1 <- number
    skip 2
    z2 <- number
    return (onOrOff == "on", (x1, x2), (y1, y2), (z1, z2))
    where
        skip = modify . drop
        while :: (Char -> Bool) -> State String String
        while f = state $ span f
        number = read <$> while (`elem` ('-' : ['0'..'9']))

solve xs (b, x, y, z)
    | b = xs <> ys
    | otherwise = xs S.\\ ys
    where
        take (a1, a2) = [max a1 (-50)..min a2 50]
        ys = S.fromList $ (,,) <$> take x <*> take y <*> take z
