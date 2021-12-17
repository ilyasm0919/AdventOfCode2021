import Control.Monad.State
import Data.Maybe (mapMaybe)
import Data.List (sortOn)
import Data.Ord (Down(Down))

main = readFile "Input17.txt" >>= print . solve . parse

parse = evalState $ do
    skip 15
    x1 <- number
    skip 2
    x2 <- number
    skip 4
    y1 <- number
    skip 2
    y2 <- number
    return ((x1, x2), (y1, y2))
    where
        skip = modify . drop
        number :: State String Int
        number = fmap read . state $ span (`elem` '-' : ['0'..'9'])

solve ((x1, x2), (y1, y2)) = fst . maximum . positions 0 . head $ okY >>= \(x, t) -> const x <$> filter (\(_, (t', b)) -> t == t' || b && t > t') okX
    where
        -- y = v + v-1 + v-2 + ... + v-t+1 = v*t - (1 + 2 + ... + t-1) = v*t - t*(t-1)/2
        -- v = y/t + (t-1)/2
        -- t <= 2*y
        okY = sortOn Down $ [1..2*abs y1] >>= \t -> (\y -> ((2*y + t*(t-1)) `div` (2*t), t)) <$> filter (\y -> (2*y + t*(t-1)) `mod` (2*t) == 0) [y1..y2]
        okX = [1..x2] >>= fmap . (,) <*> fmap ((,) <$> fst <*> snd . snd) . dropWhile ((< x1) . fst . snd) . zip [0..] . takeWhile ((<= x2) . fst) . positions 0
        positions z 0 = [(z, True)]
        positions z v = (z, False) : positions (z+v) (v-1)

