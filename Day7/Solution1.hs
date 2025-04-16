allFoldings :: [(Int -> Int -> Int)] -> [Int] -> Int -> [Int]
allFoldings _ [] 0 = []
allFoldings _ [] acc = [acc]
allFoldings [add, mul] (x:xs) acc = 
    concat [(allFoldings [add, mul] xs (add acc x)), (allFoldings [add, mul] xs (mul acc x))]