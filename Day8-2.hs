import Data.List
import Data.Maybe (fromJust)

main = readFile "Input8.txt" >>= print . sum . fmap ((\xs ->
    sum . zipWith (*) (iterate (*10) 1) . reverse . fmap
        (fromJust . (flip elemIndex . solve $ take 10 xs))
        $ drop 11 xs)
    . fmap sort . words) . lines

solve xs = fromJust $ do
    _1 <- find ((== 2) . length) xs
    _4 <- find ((== 4) . length) xs
    _7 <- find ((== 3) . length) xs
    _8 <- find ((== 7) . length) xs

    let _235 = filter ((== 5) . length) xs
    _3 <- find ((== 2) . length . intersect _1) _235
    _2 <- find ((== 2) . length . intersect _4) $ delete _3 _235
    _5 <- find (flip notElem [_2, _3]) _235

    let _069 = filter ((== 6) . length) xs
    _6 <- find ((== 1) . length . intersect _1) _069
    _9 <- find ((== 4) . length . intersect _4) _069
    _0 <- find (flip notElem [_6, _9]) _069

    return [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9]
