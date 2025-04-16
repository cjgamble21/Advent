module Solution1 where

type Rule = (Int, Int)
type Order = [Int]

type Rules = [Rule]
type Orders = [Order]

split :: Char -> String -> [String]
split _ [] = []
split delim str = substr : (split delim (drop (length substr + 1) str))
    where substr = takeWhile (/= delim) str

extractStringsWithDelimeter :: Char -> [String] -> [[String]]
extractStringsWithDelimeter delim strs = map (split delim) $ filter (\x -> x /= "" && delim `elem` x) strs

rules :: [String] -> Rules
rules = map (\[x, y] -> (read x, read y)) . (extractStringsWithDelimeter '|')

orders :: [String] -> Orders
orders = map (map read) . (extractStringsWithDelimeter ',')

elementsBefore :: Order -> Int -> Order
elementsBefore list num = takeWhile (/= num) list

contains :: Eq a => [a] -> [a] -> Bool
contains l1 l2 = length (filter (\x -> x `elem` l1) l2) > 0

afterPages :: Rules -> Int -> Order
afterPages rules print = map snd $ filter (\(a, _) -> a == print) rules

validOrder :: Rules -> Order -> Bool
validOrder rules order = not $ any (\x -> contains (afterPages rules x) (elementsBefore order x)) order

validOrders :: Rules -> Orders -> Orders
validOrders rules orders = filter (validOrder rules) orders

median :: [Int] -> Int
median list = list!!(length list `div` 2)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let parsed = lines input
        parsedRules = rules parsed
        parsedOrders = orders parsed
    print $ sum $ map median $ validOrders parsedRules parsedOrders