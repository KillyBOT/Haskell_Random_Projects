module MyHeaps.BinaryHeap
(
    HeapType(..),
    BinHeap,
    heapNew,
    heapLength,
    heapPush,
    heapPop,
    heapPushPop,
    heapDelete,
    heapIncrease,
    heapDecrease,
    heapTop,
    heapSearch,
    heapFromList,
    heapifyList,
    heapSort,
    heapMerge
) where

import Data.List

data HeapType = MinHeap | MaxHeap deriving (Show, Read, Eq)
data BinHeap o = BinHeap {heapData :: [o], lengthOfHeap :: Int, heapType :: HeapType} deriving (Show, Read, Eq)

heapNew :: (Ord o) => HeapType -> BinHeap o
heapNew heapType = BinHeap [] 0 heapType

newMinHeap :: (Ord o) => BinHeap o
newMinHeap = BinHeap [] 0 MinHeap

newMaxHeap :: (Ord o) => BinHeap o
newMaxHeap = BinHeap [] 0 MaxHeap

getVal :: (Ord o) => BinHeap o -> Int -> Maybe o
getVal (BinHeap _ 0 _) _ = Nothing
getVal heap@(BinHeap list hLength _) index
    | index < 0 = Nothing
    | index >= hLength = Nothing
    | otherwise = Just $ list !! index

getIndex :: (Ord o) => BinHeap o -> o -> Maybe Int
getIndex (BinHeap _ 0 _) _ = Nothing
getIndex heap@(BinHeap list hLength _) val = val `elemIndex` list

setVal :: (Ord o) => BinHeap o -> Int -> o -> BinHeap o
setVal heap@(BinHeap list hLength heapType) index val
    | index < 0 = heap
    | index >= hLength = heap
    | otherwise = BinHeap ((take index list) ++ [val] ++ (drop (index + 1) list)) hLength heapType

swapVals :: (Ord o) => BinHeap o -> Int -> Int -> BinHeap o
swapVals heap@(BinHeap _ 0 heapType) _ _ = heap
swapVals heap@(BinHeap list hLength _) ind1 ind2
    | ind1 < 0 = heap
    | ind1 >= hLength = heap
    | ind2 < 0 = heap
    | ind2 >= hLength = heap
    | otherwise = 
        let tempVal = list !! ind1
        in setVal (setVal heap ind1 (list !! ind2)) ind2 tempVal

getLeft :: Int -> Int
getLeft index = (index * 2) + 1

getRight :: Int -> Int
getRight index = (index * 2) + 2

getParent :: Int -> Int
getParent index = (index - 1) `quot` 2

heapLength :: (Ord o) => BinHeap o -> Int
heapLength heap@(BinHeap _ hLength _) = hLength

heapPush :: (Ord o) => o -> BinHeap o -> BinHeap o
heapPush val heap@(BinHeap list hLength MinHeap) = minUpHeap (BinHeap (list++[val]) (hLength + 1) MinHeap) hLength
heapPush val heap@(BinHeap list hLength MaxHeap) = maxUpHeap (BinHeap (list++[val]) (hLength + 1) MaxHeap) hLength

heapPushPop :: (Ord o) => o -> BinHeap o -> (o, BinHeap o)
heapPushPop val heap@(BinHeap _ 0 _) = (val, heap)
heapPushPop val heap@(BinHeap list@(x:xs) hLength MinHeap) =
    case heapTop heap of 
        Just rootVal -> if rootVal < val then (rootVal, minDownHeap (BinHeap ([val] ++ xs) hLength MinHeap) 0) 
            else (val, heap)
heapPushPop val heap@(BinHeap list@(x:xs) hLength MaxHeap) =
    case heapTop heap of 
        Just rootVal -> if rootVal < val then (rootVal, maxDownHeap (BinHeap ([val] ++ xs) hLength MaxHeap) 0)
            else (val, heap)
        
heapDelete :: (Ord o) => o -> BinHeap o -> BinHeap o
heapDelete _ heap@(BinHeap _ 0 _) = heap
heapDelete val heap@(BinHeap list hLength MinHeap) =
    case getIndex heap val of 
        Nothing -> heap
        Just index -> minUpHeap (minDownHeap (removeLast (swapVals heap index (hLength - 1))) index) index
heapDelete val heap@(BinHeap list hLength MaxHeap) =
    case getIndex heap val of 
        Nothing -> heap
        Just index -> maxUpHeap (maxDownHeap (removeLast (swapVals heap index (hLength - 1))) index) index

removeLast :: (Ord o) => BinHeap o -> BinHeap o
removeLast heap@(BinHeap _ 0 _) = heap
removeLast heap@(BinHeap list hLength heapType) = BinHeap (init list) (hLength - 1) heapType

decreaseInd :: (Ord o) => BinHeap o -> Int -> o -> BinHeap o
decreaseInd heap@(BinHeap _ 0 _) _ _ = heap
decreaseInd heap@(BinHeap _ hLength MinHeap) index val
    | index < 0 = heap
    | index >= hLength = heap
    | Just val >= (getVal heap index) = heap
    | otherwise = minUpHeap (setVal heap index val) index
decreaseInd heap@(BinHeap _ hLength MaxHeap) index val
    | index < 0 = heap
    | index >= hLength = heap
    | Just val >= (getVal heap index) = heap
    | otherwise = maxDownHeap (setVal heap index val) index

increaseInd :: (Ord o) => BinHeap o -> Int -> o -> BinHeap o
increaseInd heap@(BinHeap _ 0 _) _ _ = heap
increaseInd heap@(BinHeap _ hLength MinHeap) index val
    | index < 0 = heap
    | index >= hLength = heap
    | Just val <= (getVal heap index) = heap
    | otherwise = minDownHeap (setVal heap index val) index
increaseInd heap@(BinHeap _ hLength MaxHeap) index val
    | index < 0 = heap
    | index >= hLength = heap
    | Just val <= (getVal heap index) = heap
    | otherwise = maxUpHeap (setVal heap index val) index

heapDecrease :: (Ord o) => o -> o -> BinHeap o -> BinHeap o
heapDecrease _ _ heap@(BinHeap _ 0 _) = heap
heapDecrease val newVal heap = case getIndex heap val of
    Just x -> decreaseInd heap x newVal
    Nothing -> heap


heapIncrease :: (Ord o) => o -> o -> BinHeap o -> BinHeap o
heapIncrease _ _ heap@(BinHeap _ 0 _) = heap
heapIncrease val newVal heap = case getIndex heap val of
    Just x -> increaseInd heap x newVal
    Nothing -> heap

heapTop :: (Ord o) => BinHeap o -> Maybe o
heapTop (BinHeap _ 0 _) = Nothing
heapTop (BinHeap (x:[]) 1 _) = Just x
heapTop (BinHeap (x:xs) _ _) = Just x

heapSearch :: (Ord o) => o -> BinHeap o -> Bool
heapSearch _ (BinHeap _ 0 _) = False
heapSearch val heap@(BinHeap list _ _) = any (==val) list

removeTop :: (Ord o) => BinHeap o -> BinHeap o
removeTop heap@(BinHeap _ 0 _) = heap
removeTop heap@(BinHeap _ _ MinHeap) = minDownHeap (removeLast (swapWithEnd heap)) 0
removeTop heap@(BinHeap _ _ MaxHeap) = maxDownHeap (removeLast (swapWithEnd heap)) 0

-- I somehow doubt this is the best way of doing this...

swapWithEnd :: (Ord o) => BinHeap o -> BinHeap o
swapWithEnd heap@(BinHeap _ 0 _) = heap
swapWithEnd heap@(BinHeap _ hLength _) = swapVals heap 0 (hLength - 1)

heapPop :: (Ord o) => BinHeap o -> (Maybe o, BinHeap o)
heapPop heap = 
    let top = heapTop heap
    in (top,removeTop heap)

minUpHeap :: (Ord o) => BinHeap o -> Int -> BinHeap o
minUpHeap heap@(BinHeap _ _ MinHeap) 0 = heap
minUpHeap heap@(BinHeap list hLength MinHeap) root
    | parentVal > currentVal = minUpHeap (swapVals heap parentInd root) parentInd
    | otherwise = heap
    where
        parentInd = getParent root
        currentVal = getVal heap root
        parentVal = getVal heap parentInd

maxUpHeap :: (Ord o) => BinHeap o -> Int -> BinHeap o
maxUpHeap heap@(BinHeap _ _ MaxHeap) 0 = heap
maxUpHeap heap@(BinHeap list hLength MaxHeap) root
    | parentVal < currentVal = maxUpHeap (swapVals heap parentInd root) parentInd
    | otherwise = heap
    where
        parentInd = getParent root
        currentVal = getVal heap root
        parentVal = getVal heap parentInd

minDownHeap :: (Ord o) => BinHeap o -> Int -> BinHeap o
minDownHeap heap@(BinHeap _ 0 _) _ = heap
minDownHeap heap@(BinHeap list hLength MinHeap) root
    | root < 0 = heap
    | root >= hLength = heap
    | smallest == current = heap
    | smallest == left = minDownHeap (swapVals heap (getLeft root) root) (getLeft root)
    | otherwise = minDownHeap (swapVals heap (getRight root) root) (getRight root)
    where
        left = getVal heap (getLeft root)
        right = getVal heap (getRight root)
        current = getVal heap root
        leftRightSmallest = if left > right then right else left
        smallest = if leftRightSmallest >= current then current else leftRightSmallest

maxDownHeap :: (Ord o) => BinHeap o -> Int -> BinHeap o
maxDownHeap heap@(BinHeap _ 0 _) _ = heap
maxDownHeap heap@(BinHeap list hLength MaxHeap) root
    | root < 0 = heap
    | root >= hLength = heap
    | largest == current = heap
    | largest == left = maxDownHeap (swapVals heap leftInd root) leftInd
    | otherwise = maxDownHeap (swapVals heap rightInd root) rightInd
    where
        leftInd = getLeft root
        rightInd = getRight root
        left = getVal heap leftInd
        right = getVal heap rightInd
        current = getVal heap root
        leftRightLargest = if left < right then right else left
        largest = if leftRightLargest <= current then current else leftRightLargest

heapFromList :: (Ord o) => [o] -> HeapType -> BinHeap o
heapFromList [] heapType = heapNew heapType
heapFromList list heapType = foldl (flip heapPush) (heapNew heapType) list

heapifyList :: (Ord o) => [o] -> HeapType -> [o]
heapifyList [] _ = []
heapifyList list heapType = heapGetList (heapFromList list heapType)

heapGetList :: (Ord o) => BinHeap o -> [o]
heapGetList heap@(BinHeap list _ _) = list

heapSort :: (Ord o) => [o] -> [o]
heapSort [] = []
heapSort list = heapSortHelp (heapFromList list MinHeap) []

heapMerge :: (Ord o) => BinHeap o -> BinHeap o -> HeapType -> BinHeap o
heapMerge heap1 (BinHeap _ 0 _) _ = heap1
heapMerge (BinHeap _ 0 _) heap2 _ = heap2
heapMerge heap1 heap2 heapType = heapFromList (heapGetList heap1 ++ heapGetList heap2) heapType

heapSortHelp :: (Ord o) => BinHeap o -> [o] -> [o]
heapSortHelp heap@(BinHeap _ 0 _) list = list
heapSortHelp heap list = case fst popped of
    Just x -> heapSortHelp (snd popped) (list ++ [x])
    where popped = heapPop heap

{-
printHeap :: (Ord o) => BinHeap o -> String
printHeap heap = printHeapHelp heap 0 ""

printHeapHelp :: (Show o) => BinHeap o -> Int -> String -> String
printHeapHelp heap@(BinHeap list hLength _) root tabs
    | root >= hLength = ""
    | otherwise = tabs ++ (show (getVal heap root)) ++ "\n" ++ printHeapHelp heap (getLeft root) (tabs ++ "\t") ++ printHeapHelp heap (getRight root) (tabs ++ "\t")
-}