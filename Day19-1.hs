import Control.Monad.State
import Data.Either (partitionEithers)
import Data.List (intersect)
import Data.Maybe (listToMaybe)
import qualified Data.Set as S

main = readFile "Input19.txt" >>= print . S.size . (solve <$> pure . flip (,) id . head <*> tail) . parse . tail . lines

parse [] = [[]]
parse ("" : _ : xs) = [] : parse xs
parse (s : xs) = (parseLine s : y) : ys
    where y : ys = parse xs

parseLine = evalState $ do
    x <- number
    modify tail
    y <- number
    modify tail
    z <- number
    return (x, y, z)
    where
        number :: State String Int
        number = fmap read . state $ span (`elem` '-' : ['0'..'9'])

rotations =
    [ \(x, y, z) -> ( x,  y,  z)
    , \(x, y, z) -> ( x,  z, -y)
    , \(x, y, z) -> ( x, -y, -z)
    , \(x, y, z) -> ( x, -z,  y)

    , \(x, y, z) -> (-x,  y, -z)
    , \(x, y, z) -> (-x, -z, -y)
    , \(x, y, z) -> (-x, -y,  z)
    , \(x, y, z) -> (-x,  z,  y)

    , \(x, y, z) -> ( y,  x, -z)
    , \(x, y, z) -> ( y, -z, -x)
    , \(x, y, z) -> ( y, -x,  z)
    , \(x, y, z) -> ( y,  z,  x)

    , \(x, y, z) -> (-y,  x,  z)
    , \(x, y, z) -> (-y,  z, -x)
    , \(x, y, z) -> (-y, -x, -z)
    , \(x, y, z) -> (-y, -z,  x)
    
    , \(x, y, z) -> ( z,  x,  y)
    , \(x, y, z) -> ( z,  y, -x)
    , \(x, y, z) -> ( z, -x, -y)
    , \(x, y, z) -> ( z, -y,  x)

    , \(x, y, z) -> (-z,  x, -y)
    , \(x, y, z) -> (-z, -y, -x)
    , \(x, y, z) -> (-z, -x,  y)
    , \(x, y, z) -> (-z,  y,  x) ]

tryIntersect xs ys = listToMaybe $ do
    r <- rotations
    let ys' = r <$> ys
    y <- ys'
    t <- translation y <$> xs
    guard . (>= 12) . S.size $ S.fromList xs `S.intersection` S.fromList (fmap t ys')
    return (t . r)
    where translation (x1, y1, z1) (x2, y2, z2) (x, y, z) = (x + x2 - x1, y + y2 - y1, z + z2 - z1)

solve xs [] = foldMap (S.fromList . uncurry (flip fmap)) xs
solve xs ys = S.union (foldMap (S.fromList . uncurry (flip fmap)) xs) $ solve ys1 ys2
    where (ys1, ys2) = partitionEithers $ fmap (\y -> maybe (Right y) (Left . (,) y) . msum $ fmap (\(x, f) -> (f .) <$> x `tryIntersect` y) xs) ys
