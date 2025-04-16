type PairOfLists = ([Int], [Int])
type ListOfPairs = [(Int, Int)]
type Lines = [[String]]

rowsToColumns :: Lines -> PairOfLists
rowsToColumns row = unzip [(read a, read b) | [a, b] <- row]

similarityScore :: Int -> [Int] -> Int
similarityScore num nums = num * (length $ filter (== num) nums)

totalSimilarityScore :: PairOfLists -> Int
totalSimilarityScore (a, b) = sum $ map (\num -> similarityScore num b) a

solve :: Lines -> Int
solve = totalSimilarityScore . rowsToColumns

main :: IO ()
main =
    putStrLn. show . solve . map words . lines =<< readFile "input.txt"