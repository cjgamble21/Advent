import Data.Char
import qualified Data.Set as Set

type Index = (Int, Int)

type Indexed a = (Index, a)

type Matrix a = [[a]]

type InitialMatrix = Matrix Char
type ParsedMatrix = [Indexed Int]
matrix :: InitialMatrix = [
    "...0...",
    "...10..",
    "...2...",
    "6543456",
    "7.....7",
    "8.....8",
    "9.....9"]

mkUniq :: Ord a => [a] -> [a]
mkUniq = Set.toList . Set.fromList

parseMatrix :: InitialMatrix -> ParsedMatrix
parseMatrix m = concat [ [ ((x, y), digitToInt item) | (x, item) <- zip [0..] row, isDigit item ] | (y, row) <- zip [0..] m]

getTrailheads :: ParsedMatrix -> ParsedMatrix
getTrailheads = filter ((== 0) . snd)

areIndicesAdjacent :: Index -> Index -> Bool
areIndicesAdjacent (x1, y1) (x2, y2) =
    abs (x1 - x2) == 1 && abs (y1 - y2) == 0 || abs (y1 - y2) == 1 && abs (x1 - x2) == 0

isAdjacentMatch :: Indexed Int -> Indexed Int -> Bool
isAdjacentMatch (headIndex, head) (toMatchIndex, toMatch) =
    toMatch == head + 1 && areIndicesAdjacent headIndex toMatchIndex

numPathsForTrailhead :: ParsedMatrix -> Indexed Int -> [Index]
numPathsForTrailhead _ (index, 9) = [index]
numPathsForTrailhead matrix item = (concatMap (numPathsForTrailhead matrix) (filter (isAdjacentMatch item) matrix))
    
solve :: ParsedMatrix -> [(Index, Index)]
solve m = concat $ map (\value@(index, _) -> zipWith (,) (repeat index) (numPathsForTrailhead m value)) trailheads
    where trailheads = getTrailheads m

main :: IO ()
main = do
    input <- readFile "input.txt"
    let matrix = lines input
    print $ length $ solve (parseMatrix matrix)