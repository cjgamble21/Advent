import Data.List
import Data.Monoid

type Grid = [String]

xmasCount :: String -> Int
xmasCount str@(_:xs)
    | "XMAS" `isPrefixOf` str = 1 + xmasCount xs
    | otherwise = xmasCount xs
xmasCount _ = 0

fullCount :: Grid -> Int
fullCount grid = sum (map xmasCount grid) + sum (map (xmasCount . reverse) grid)

rows :: Grid -> Grid
rows = id

cols :: Grid -> Grid
cols = transpose

diagsFromOffset :: Grid -> Int -> Grid
diagsFromOffset grid offset = 
    [zipWith (!!) grid [offset..(length grid - 1)]]

allDiags :: Grid -> Grid
allDiags grid = concatMap (\offset -> diagsFromOffset grid offset) [0..(length grid - 1)]


diagonals :: Grid -> Grid
diagonals grid = 
    allDiags grid ++ allDiags (transpose grid)

-- Extract diagonals traversing from top-left to bottom-right (major diagonal)
primaryDiagonals :: Grid -> Grid
primaryDiagonals = allDiags

-- Extract diagonals traversing from top-right to bottom-left (minor diagonal)
secondaryDiagonals :: Grid -> Grid
secondaryDiagonals = allDiags . reverse . transpose

countAllOccurences :: Grid -> Int
countAllOccurences = getSum . foldMap (Sum . fullCount) . sequence [rows, cols, primaryDiagonals, secondaryDiagonals]

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ countAllOccurences $ lines input

    -- zipWith (!!) grid [0..(length grid - 1)] <-- MAIN DIAGONAL
    -- zipWith (!!) grid [1..(length grid - 1)]
    -- zipWith (!!) grid [2..(length grid - 1)]

    -- ([a] -> b -> [c]) -> [[a]] -> [b] -> [[c]]