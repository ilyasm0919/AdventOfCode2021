import Control.Monad.State
import Data.Bool (bool)
import Data.List (sortOn)
import qualified Data.Set as S

main = readFile "Input22.txt" >>= print . solve . S.fromAscList . zipWith parse [0..] . lines

parse n = evalState $ do
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
    return $ Cube { number = n, isOn = onOrOff == "on", x = (x1, x2 + 1), y = (y1, y2 + 1), z = (z1, z2 + 1) }
    where
        skip = modify . drop
        while :: (Char -> Bool) -> State String String
        while f = state $ span f
        number :: State String Int
        number = read <$> while (`elem` ('-' : ['0'..'9']))

data Cube = Cube { number :: Int, isOn :: Bool, x :: (Int, Int), y :: (Int, Int), z :: (Int, Int) } deriving (Eq, Ord)
data Event = Event { startCube :: Bool, pos :: Int, cube :: Cube }

mkEvents :: (Cube -> (Int, Int)) -> S.Set Cube -> [Event]
mkEvents f = sortOn pos . concatMap (\x -> let (pos1, pos2) = f x in [Event { startCube = True, pos = pos1, cube = x }, Event { startCube = False, pos = pos2, cube = x }])

foldEvents :: (S.Set Cube -> Int) -> [Event] -> Int
foldEvents f xs = snd . foldl (\(s, n) (e1, e2) ->
    let op | startCube e2 = S.insert
           | otherwise = S.delete in
    n `seq` (op (cube e2) s, n + f s * (pos e2 - pos e1))) (mempty, 0) $ zip (Event{pos = 0} : xs) xs

foldCubes :: (Cube -> (Int, Int)) -> (S.Set Cube -> Int) -> S.Set Cube -> Int
foldCubes f g = foldEvents g . mkEvents f

solve :: S.Set Cube -> Int
solve = foldCubes x . foldCubes y . foldCubes z $ maybe 0 (bool 0 1 . isOn) . S.lookupMax
