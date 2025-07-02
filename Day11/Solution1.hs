newtype Stone = Stone {
   getNumber :: Int
} deriving (Eq, Show)

splitList :: [a] -> [[a]]
splitList list = [take midpoint list, drop midpoint list]
    where len = length list
          midpoint = len `div` 2

shouldSplitStone :: Stone -> Bool
shouldSplitStone = (== 0) . (`mod` 2) . length . show . getNumber

splitStone :: Stone -> [Stone]
splitStone stone = map Stone $ map read $ splitList $ show $ getNumber stone

blink :: Stone -> [Stone]
blink stone@(Stone num)
    | num == 0 = [Stone 1]
    | shouldSplitStone stone = splitStone stone
    | otherwise = [Stone (num * 2024)]

stones :: [Stone]
stones = [Stone 0, Stone 1, Stone 10, Stone 99, Stone 999]

solve :: [Stone] -> Int -> Int
solve stones 0 = length stones
solve stones left = solve (concatMap blink stones) (left - 1)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let values = words input
    let stones = map Stone $ map read $ values
    print $ solve stones 75
