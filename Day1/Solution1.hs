import Data.List

type PairOfLists = ([Int], [Int])
type ListOfPairs = [(Int, Int)]
type Lines = [[String]]

difference :: (Int, Int) -> Int
difference (a, b) = abs $ a - b

sumPairs :: ListOfPairs -> Int
sumPairs = foldl (+) 0 . map difference

rowsToColumns :: Lines -> PairOfLists
rowsToColumns row = unzip [(read a, read b) | [a, b] <- row]

sortColumns :: PairOfLists -> PairOfLists
sortColumns (a, b) = (sort a, sort b)

getPairs :: PairOfLists -> ListOfPairs
getPairs (a, b) = zip a b

solve :: Lines -> Int
solve = sumPairs . getPairs . sortColumns . rowsToColumns

main :: IO ()
main =
    putStrLn . show . solve. map words . lines =<< readFile "input.txt"