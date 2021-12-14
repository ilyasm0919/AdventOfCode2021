import Data.List (sort, group)
import Data.Maybe (maybeToList)
import qualified Data.Map as M

main = do
    ((h : x) : "" : xs) <- lines <$> readFile "Input14.txt"
    let start = M.fromListWith (+) . zip (zip (h : x) x) $ repeat 1
        m = M.fromList $ fmap (\(y : z : end) -> ((y, z), last end)) xs
        res = foldr (.) id . replicate 40 $ solve m
        ys = M.elems . M.unionsWith (+) . (M.singleton h 1:) . fmap (uncurry $ M.singleton . snd) . M.toList $ res start
    print $ maximum ys - minimum ys

solve m = M.unionsWith (+) . M.mapWithKey (\(x, z) n -> let y = m M.! (x, z) in M.fromListWith (+) . zip [(x, y), (y, z)] $ repeat n)
