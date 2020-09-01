import System.Random
seed = randomIO :: IO Int
randList = take 30 (randomRs (1,100) (mkStdGen 2123) :: [Integer])

mMap :: (a -> a) -> [a] -> [a]
mMap _ [] = []
mMap f (x:xs) = f x : mMap f xs

mZipWith :: (a->b->c) -> [a] -> [b] -> [c]
mZipWith _ [] _ = []
mZipWith _ _ [] = []
mZipWith f (x:xs) (y:ys) = f x y : mZipWith f xs ys

mFilter :: (a -> Bool) -> [a] -> [a]
mFilter _ [] = []
mFilter f (x:xs)
    | (f x) == True = x : mFilter f xs
    | otherwise = mFilter f xs

mLongerThan :: Int -> [a] -> Bool
mLongerThan desiredLength list = (length list > desiredLength)

mTakeWhile :: (a -> Bool) -> [a] -> [a]
mTakeWhile _ [] = []
mTakeWhile f (x:xs)
    | (f x) == True = x : mTakeWhile f xs
    | otherwise = [] 

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

mTranspose :: [[a]] -> [[a]]
mTranspose x = init $ mTransposeHelper x

mTransposeHelper :: [[a]] -> [[a]]
mTransposeHelper [] = []
mTransposeHelper x = [mTransposeGetHead x] ++ (mTransposeHelper (mTransposeGetTail x))

mTransposeGetHead :: [[a]] -> [a]
mTransposeGetHead [] = []
mTransposeGetHead ([]:xs) = mTransposeGetHead xs
mTransposeGetHead (x:xs) = head x : mTransposeGetHead xs

mTransposeGetTail :: [[a]] -> [[a]]
mTransposeGetTail [] = []
mTransposeGetTail ([]:xs) = mTransposeGetTail xs
mTransposeGetTail (x:xs) = tail x : mTransposeGetTail xs
