-- It doesn't work and I have no ideas how to solve it
import Control.Monad (guard)
import Data.Maybe (isJust)
import Data.List (splitAt, transpose, find)
import Data.Char (ord)

type Hallway = Int
type Room = Int
type Amphipod = Room
type HorPos = Int
data State = State { cost :: Int, hallway :: [Maybe Amphipod], rooms :: [Either [Amphipod] Int] } deriving (Eq, Ord)

goal = 2--4

horHall :: Hallway -> HorPos
horHall a = a*2
horRoom :: Room -> HorPos
horRoom a = a*2+3

a `between` (b, c) = min b c < a && a < max b c

(!!=) :: [a] -> (Int, a) -> [a]
xs !!= (pos, x) = let (l, _:r) = splitAt pos xs in l <> [x] <> r

moveCost :: Amphipod -> Int
moveCost n = 10 ^ n

move :: State -> [State]
move s = do
    (hall, Just amphipod) <- zip [0..] $ hallway s
    Right n <- return $ rooms s !! amphipod
    canMove(horHall hall, horRoom amphipod)
    return $ State
        { cost = cost s + moveCost amphipod * (abs (horHall hall - horRoom amphipod) + goal - n)
        , hallway = hallway s !!= (hall, Nothing)
        , rooms = rooms s !!= (amphipod, Right $ succ n)
        }
    <> do
    (room, Left (amphipod : xs)) <- zip [0..] $ rooms s
    (hall, Nothing) <- zip [0..] $ hallway s
    canMove(horRoom room, horHall hall)
    return $ State
        { cost = cost s + moveCost amphipod * (abs (horRoom room - horHall hall) + goal - length xs)
        , hallway = hallway s !!= (hall, Just amphipod)
        , rooms = rooms s !!= (room, if null xs then Right 0 else Left xs)
        }
    where canMove bounds = guard . not . any (\(i, a) -> isJust a && horHall i `between` bounds) . zip [0..] $ hallway s

solve :: [State] -> Int
solve xs
    | Just s <- find (all (== Right goal) . rooms) xs = cost s
    | otherwise = solve $ xs >>= move

main = readFile "Input23.txt" >>= print . solve . return . State 0 (replicate 7 Nothing) . fmap Left
    . fmap (fmap $ subtract (ord 'A') . ord) . transpose . insertLines . fmap (filter (`elem` "ABCD")) . take 2 . drop 2 . lines

insertLines [x, y] = [x, y]--[x, "DCBA", "DBAC", y]
