-- I don't want to use external dependencies (like containers) in AOC, but here it is very helpful
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (isUpper)
import Data.Maybe (fromMaybe)

main = readFile "Input12.txt" >>= print . solve True S.empty "start" . M.fromListWith (++) . concatMap (\l ->
    let a = takeWhile (/= '-') l
        b = tail $ dropWhile (/= '-') l in
    [(a, [b]), (b, [a])]) . lines

solve _ _ "end" = return 1
solve _ s "start" | not $ S.null s = return 0
solve b s v
    | isUpper (head v) = fromMaybe [] . M.lookup v >>= fmap sum . traverse (solve b s)
    | v `S.member` s = if b then fromMaybe [] . M.lookup v >>= fmap sum . traverse (solve False s) else return 0
    | otherwise = fromMaybe [] . M.lookup v >>= fmap sum . traverse (solve b $ S.insert v s)
