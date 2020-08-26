import System.Random
seed = randomIO :: IO Int
randList = take 30 (randomRs (1,100) (mkStdGen 2123) :: [Integer])

setVal :: (Ord o) => [o] -> Int -> o -> [o]
setVal list index rep
    | index < 0 = list
    | index >= length list = list
    | otherwise = (take index list) ++ [rep] ++ (drop (index + 1) list)

swapVals :: (Ord o) => [o] -> Int -> Int -> [o]
swapVals list ind1 ind2
    | ind1 < 0 = list
    | ind2 < 0 = list
    | ind1 >= length list = list
    | ind2 >= length list = list
    | otherwise = 
        let tempVal = list !! ind1
        in setVal (setVal list ind1 (list !! ind2)) ind2 tempVal

mergeSortedLists :: (Ord a) => [a] -> [a] -> [a]
mergeSortedLists x [] = x
mergeSortedLists [] y = y
mergeSortedLists xAll@(x:xs) yAll@(y:ys)
    | x < y = [x] ++ mergeSortedLists xs yAll
    | otherwise = [y] ++ mergeSortedLists xAll ys

mergeSortHelper :: (Ord a) => [a] -> Int -> [a]
mergeSortHelper n 1 = n
mergeSortHelper n l = mergeSortedLists (mergeSortHelper (fst splitList) l1) (mergeSortHelper (snd splitList) l2)
    where
        l1 = l `div` 2
        l2 = l - l1
        splitList = splitAt (l1) n

mergeSort :: (Ord a) => [a] -> [a]
mergeSort n = mergeSortHelper n (length n)


quickSort :: (Ord a) => [a] -> [a]
quickSort [] = [] 
quickSort (x:xs) = 
    let smallerThan = quickSort [n | n <- xs, n <= x]
        biggerThan = quickSort [n | n <- xs, n > x]
    in smallerThan ++ [x] ++ biggerThan
