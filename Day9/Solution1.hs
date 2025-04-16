data DiskSlot a = ID a | Open deriving (Show, Eq)

list :: [Int]
list = [1, 2, 3, 4, 5]

splitList :: [a] -> ([a], [a])
splitList = foldr (\x ~(ys,zs) -> (x:zs,ys)) ([],[])

zipLists :: [a] -> [a] -> [a]
zipLists [] ys = ys
zipLists xs [] = xs
zipLists (x:xs) (y:ys) = x : y : zipLists xs ys

generateIDList :: [Int] -> [[DiskSlot Int]]
generateIDList ids = [replicate amount (ID id) | (amount, id) <- zip ids [0..]]

buildMemoryMap :: [Int] -> [[DiskSlot Int]]
buildMemoryMap memMap = 
  let (ids, freeZones) = splitList memMap
  in zipWith (++) (generateIDList ids) (map (\zones -> (replicate zones Open)) freeZones)