import Data.Char
import Data.List
import qualified Data.Set as Set

type Point = (Int, Int)

type Id = Char

data Antenna = Antenna Id Point deriving Show

type Grid = [Antenna]
type GroupedAntennas = [Grid]

type GridSize = Point

mkUniq :: Ord a => [a] -> [a]
mkUniq = Set.toList . Set.fromList

gridSize :: [String] -> GridSize
gridSize grid = (length $ head grid, length grid)

isValidPoint :: GridSize -> Point -> Bool
isValidPoint (colLength, rowLength) (colIdx, rowIdx)  =
    (colIdx >= 0 && colIdx < colLength) && (rowIdx >= 0 && rowIdx < rowLength)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

allGroupAntiNodes :: Grid -> [Point]
-- allGroupAntiNodes = concatMap antiNodes . pairs

bothPointsAreValid :: (Point, Point) -> Bool
bothPointsAreValid ()

allAntiNodes :: (Antenna, Antenna) -> [Point]
allAntiNodes = takeWhile antiNodes 1

antiNodes :: Int -> (Antenna, Antenna) -> [Point]
antiNodes factor (a1@(Antenna _ (x1, y1)), a2@(Antenna _ (x2, y2))) =
    [[(x1 + xDistance, y1 + yDistance), (x2 - xDistance, y2 - yDistance)]]
    ++ [antiNodes (factor + 1) (a1, a2)]
    where yDistance = (y1 - y2) * factor
          xDistance = (x1 - x2) * factor
    
antennasAreEqual :: Antenna -> Antenna -> Bool
antennasAreEqual (Antenna a _) (Antenna b _) = a == b

groupAntennasById :: Grid -> GroupedAntennas
groupAntennasById = groupBy antennasAreEqual . sortOn (\(Antenna id _) -> id)

isAntenna :: Antenna -> Bool
isAntenna (Antenna id _) = isAlphaNum id

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex = flip zipWith [0..]

createGrid :: [String] -> Grid
createGrid = concat . map (filter isAntenna) . mapWithIndex (\rowIdx row -> mapWithIndex (\colIdx id -> Antenna id (colIdx, rowIdx)) row)

solve :: Grid -> GridSize -> Int
solve grid size = length $ mkUniq $ filter (isValidPoint size) $ concatMap allGroupAntiNodes $ groupAntennasById grid

main :: IO ()
main = do
    input <- readFile "input.txt"
    let grid = createGrid $ lines input
    let size = gridSize $ lines input
    print $ solve grid size
