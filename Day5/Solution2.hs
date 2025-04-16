module Solution2 where

import Solution1
import Data.List

halved :: [a] -> ([a], [a])
halved list = splitAt half list
  where half = length list `div` 2

merge :: Ord a => (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ xs [] = xs
merge _ [] ys = ys
merge pred (x:xs) (y:ys)
  | pred x y = x : merge pred xs (y:ys)
  | otherwise = y : merge pred (x:xs) ys

msort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
msort _ [] = []
msort _ [x] = [x]
msort pred xs = merge pred (msort pred (fst halves)) (msort pred (snd halves))
    where halves = halved xs

orderValid :: Rules -> (Int -> Int -> Bool)
orderValid rules = \x y -> not $ x `elem` (afterPages rules y)

invalidOrders :: Rules -> Orders -> Orders
invalidOrders rules orders = filter (not . validOrder rules) orders

sortOrder :: Rules -> Order -> Order
sortOrder rules order = msort (orderValid rules) order

main2 :: IO ()
main2 = do
    input <- readFile "input.txt"
    let parsed = lines input
        parsedRules = rules parsed
        parsedOrders = orders parsed
    print $ sum $ map median $ map (sortOrder parsedRules) $ invalidOrders parsedRules parsedOrders