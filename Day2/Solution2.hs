import Data.List

inOrder :: Ord a => [a] -> Bool
inOrder list = (list == (sort list)) || (list == (reverse $ sort list))

isCorrectDifference :: (Int, Int) -> Bool
isCorrectDifference (x, y) = diff >= 1 && diff <= 3
    where diff = abs $ x - y

overlappingPairs :: [a] -> [(a, a)]
overlappingPairs list = zip list (tail list)

stringToInt :: [[String]] -> [[Int]]
stringToInt = map $ map read

safe :: [Int] -> Bool
safe list = inOrder list && (all isCorrectDifference $ overlappingPairs list)

getSubLists :: [Int] -> [[Int]]
getSubLists [] = [[]]
getSubLists list = [list] ++ (zipWith (++) (inits list) (tail (tails list)))

solve :: [[String]] -> Int
solve = length . filter (any safe) . map getSubLists . stringToInt

main :: IO ()
main = 
    putStrLn . show . solve . map words . lines =<< readFile "input.txt"

    