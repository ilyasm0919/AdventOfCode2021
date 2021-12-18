import Control.Applicative ((<|>), liftA2)
import Control.Monad.State
import Data.Char (isDigit)
import Data.Maybe (catMaybes)

data Snailfish = Number Int | Pair Snailfish Snailfish deriving Eq

parseSnailfish = do
    s <- gets head
    if s == '['
        then Pair <$ modify tail <*> parseSnailfish <* modify tail <*> parseSnailfish <* modify tail
        else fmap (Number . read) . state $ span isDigit

reduce n = maybe n reduce $ explode n [] <|> split n

explode (Pair (Number a) (Number b)) xs | length xs >= 4 = Just $ foldBack True True (Number 0) xs
    where
        addL (Number x) = Number $ a + x
        addL (Pair x y) = Pair x (addL y)

        addR (Number x) = Number $ b + x
        addR (Pair x y) = Pair (addR x) y

        foldBack _ _ x [] = x
        foldBack l r x (Left  y : xs) = foldBack False r (Pair (if l then addL y else y) x) xs
        foldBack l r x (Right y : xs) = foldBack l False (Pair x (if r then addR y else y)) xs
explode (Pair x y) xs = explode x (Right y : xs) <|> explode y (Left x : xs)
explode _ _ = Nothing

split (Number n) | n >= 10 = Just $ Pair (Number $ n `div` 2) (Number $ n - n `div` 2)
split (Pair x y) = flip Pair y <$> split x <|> Pair x <$> split y
split _ = Nothing

magnitude (Number n) = n
magnitude (Pair x y) = magnitude x * 3 + magnitude y * 2

main = readFile "Input18.txt" >>= print . maximum . catMaybes . join (liftA2 $ \x y -> if x == y then Nothing else Just . magnitude . reduce $ Pair x y) . fmap (evalState parseSnailfish) . lines
