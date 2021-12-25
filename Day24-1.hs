-- It doesn't work
-- I found some pattern in input (see Day24-pattern.txt) but I have no ideas how to use it
import Control.Monad.State
import Data.Maybe (fromMaybe, fromJust)
import Data.List (elemIndex)

main = readFile "Input24.txt" >>= traverse print . solve . fmap parse . lines

type Var = Int
type Op = Integer -> Integer -> Integer
data Instr = Input Var | Op Op Var (Either Var Integer)

parse = evalState $ do
    op <- word
    if op == "inp"
        then Input <$> var
        else Op (fromJust $ op `lookup` zip ["add", "mul", "div", "mod", "eql"] [(+), (*), div, mod, \x y -> if x == y then 1 else 0]) <$> var <*> varOrInt
    where
        word = state $ fmap tail . span (/= ' ')
        mkVar = flip elemIndex "wxyz" . head
        var = fromJust . mkVar <$> word
        varOrInt = fmap (fromMaybe <$> Right . read <*> fmap Left . mkVar) word

solve xs = fmap (\input -> (input, last (fst $ foldl run (replicate 4 0, input) xs))) inputs
    where inputs = replicateM 14 [9,8..1]

(!!=) :: [a] -> (Int, a) -> [a]
xs !!= (pos, x) = let (l, _:r) = splitAt pos xs in l <> [x] <> r

run (vs, x : xs) (Input v) = (vs !!= (v, x), xs)
run (vs, xs) (Op f x y) = (vs !!= (x, f (vs !! x) (either (vs !!) id y)), xs)
