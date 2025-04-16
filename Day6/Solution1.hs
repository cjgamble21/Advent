import Data.List
import Data.Maybe

type Point = (Int, Int)

data Cell = Tile | Visited | Obstacle | Guard Orientation deriving (Show, Eq)

data Orientation = North | South | West | East deriving (Show, Eq)

type Grid = [[Cell]]

matrixIndex :: Eq a => a -> [[a]] -> Maybe Point
matrixIndex elem matrix = listToMaybe 
    [ (rowIndex, colIndex)| (rowIndex, row) <- zip [0..] matrix, Just colIndex <- [elemIndex elem row]]

guard :: Grid -> Cell
guard = head . head . map (filter isGuard)
    where isGuard (Guard _) = True
          isGuard _ = False

-- 90 degree rotation
rotate :: Orientation -> Orientation
rotate North = East
rotate East = South
rotate South = West
rotate West = North

orientation :: Char -> Orientation
orientation c
    | c == '^' = North
    | c == 'v' = South
    | c == '>' = East
    | c == '<' = West

createCell :: Char -> Cell
createCell c
    | c == '.' = Tile
    | c == '#' = Obstacle
    | c == 'X' = Visited
    | otherwise = Guard (orientation c)

buildGrid :: [String] -> Grid
buildGrid = map (map createCell)

hasLeftGrid :: Grid -> Point -> Bool
hasLeftGrid grid (x, y) = not $ x `elem` [0..rowLength - 1] && y `elem` [0..colLength - 1]
    where rowLength = length grid
          colLength = length (head grid)

move :: State Point Orientation