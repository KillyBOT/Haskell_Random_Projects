import Data.List

data HeapType = MinHeap | MaxHeap deriving (Show, Read, Eq)
data BinHeap o = BinHeap {heapData :: [o], heapLength :: Int, heapType :: HeapType} deriving (Show, Read, Eq)

emptyHeap = BinHeap {heapData = [], heapLength = 0, heapType = MinHeap}

newHeap :: (Ord o) => HeapType -> BinHeap o
newHeap heapType = BinHeap [] 0 heapType

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
swapVals (BinHeap _ 0 heapType) _ _ = newHeap heapType
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

heapAdd :: (Ord o) => o -> BinHeap o -> BinHeap o
heapAdd val heap@(BinHeap list hLength MinHeap) = fixMinHeap (BinHeap (list++[val]) (hLength + 1) MinHeap) hLength
heapAdd val heap@(BinHeap list hLength MaxHeap) = fixMaxHeap (BinHeap (list++[val]) (hLength + 1) MaxHeap) hLength

heapDelete :: (Ord o) => o -> BinHeap o -> BinHeap o
heapDelete _ heap@(BinHeap _ 0 _) = heap
heapDelete val heap@(BinHeap list hLength MinHeap) =
    case getIndex heap val of 
        Nothing -> heap
        Just index -> fixMinHeap (minHeapify (removeLast (swapVals heap index (hLength - 1))) index) index
heapDelete val heap@(BinHeap list hLength MaxHeap) =
    case getIndex heap val of 
        Nothing -> heap
        Just index -> fixMaxHeap (maxHeapify (removeLast (swapVals heap index (hLength - 1))) index) index


removeLast :: (Ord o) => BinHeap o -> BinHeap o
removeLast heap@(BinHeap _ 0 _) = heap
removeLast heap@(BinHeap list hLength heapType) = BinHeap (init list) (hLength - 1) heapType

decreaseKey :: (Ord o) => BinHeap o -> Int -> o -> BinHeap o
decreaseKey (BinHeap _ 0 heapType) _ _ = newHeap heapType
decreaseKey heap@(BinHeap _ hLength MinHeap) index val
    | index < 0 = heap
    | index >= hLength = heap
    | Just val >= (getVal heap index) = heap
    | otherwise = fixMinHeap (setVal heap index val) index
decreaseKey heap@(BinHeap _ hLength MaxHeap) index val
    | index < 0 = heap
    | index >= hLength = heap
    | Just val >= (getVal heap index) = heap
    | otherwise = maxHeapify (setVal heap index val) index

increaseKey :: (Ord o) => BinHeap o -> Int -> o -> BinHeap o
increaseKey (BinHeap _ 0 heapType) _ _ = newHeap heapType
increaseKey heap@(BinHeap _ hLength MinHeap) index val
    | index < 0 = heap
    | index >= hLength = heap
    | Just val <= (getVal heap index) = heap
    | otherwise = minHeapify (setVal heap index val) index
increaseKey heap@(BinHeap _ hLength MaxHeap) index val
    | index < 0 = heap
    | index >= hLength = heap
    | Just val <= (getVal heap index) = heap
    | otherwise = fixMaxHeap (setVal heap index val) index

getTop :: (Ord o) => BinHeap o -> Maybe o
getTop (BinHeap _ 0 _) = Nothing
getTop (BinHeap (x:[]) 1 _) = Just x
getTop (BinHeap (x:xs) _ _) = Just x

removeTop :: (Ord o) => BinHeap o -> BinHeap o
removeTop (BinHeap _ 0 heapType) = newHeap heapType
removeTop heap@(BinHeap (x:xs) hLength MinHeap) = minHeapify (BinHeap xs (hLength - 1) MinHeap) 0
removeTop heap@(BinHeap (x:xs) hLength MaxHeap) = maxHeapify (BinHeap xs (hLength - 1) MaxHeap) 0

heapPop :: (Ord o) => BinHeap o -> (Maybe o, BinHeap o)
heapPop heap = 
    let top = getTop heap
    in (top,removeTop heap)

fixMinHeap :: (Ord o) => BinHeap o -> Int -> BinHeap o
fixMinHeap heap@(BinHeap _ _ MinHeap) 0 = heap
fixMinHeap heap@(BinHeap _ _ MaxHeap) _ = heap
fixMinHeap heap@(BinHeap list hLength MinHeap) root
    | parentVal > currentVal = fixMinHeap (swapVals heap parentInd root) parentInd
    | otherwise = heap
    where
        parentInd = getParent root
        currentVal = getVal heap root
        parentVal = getVal heap parentInd

fixMaxHeap :: (Ord o) => BinHeap o -> Int -> BinHeap o
fixMaxHeap heap@(BinHeap _ _ MaxHeap) 0 = heap
fixMaxHeap heap@(BinHeap _ _ MinHeap) _ = heap
fixMaxHeap heap@(BinHeap list hLength MaxHeap) root
    | parentVal <= currentVal = fixMaxHeap (swapVals heap parentInd root) parentInd
    | otherwise = heap
    where
        parentInd = getParent root
        currentVal = getVal heap root
        parentVal = getVal heap parentInd

minHeapify :: (Ord o) => BinHeap o -> Int -> BinHeap o
minHeapify heap@(BinHeap _ 0 _) _ = heap
minHeapify heap@(BinHeap _ _ MaxHeap) _ = heap
minHeapify heap@(BinHeap list hLength MinHeap) root
    | root < 0 = heap
    | root >= hLength = heap
    | smallest == left = minHeapify (swapVals heap (getLeft root) root) (getLeft root)
    | smallest == right = minHeapify (swapVals heap (getRight root) root) (getRight root)
    | otherwise = heap
    where
        left = getVal heap (getLeft root)
        right = getVal heap (getRight root)
        current = getVal heap root
        leftRightSmallest = if left > right then right else left
        smallest = if leftRightSmallest > current then current else leftRightSmallest

maxHeapify :: (Ord o) => BinHeap o -> Int -> BinHeap o
maxHeapify heap@(BinHeap _ 0 _) _ = heap
maxHeapify heap@(BinHeap _ _ MinHeap) _ = heap
maxHeapify heap@(BinHeap list hLength MaxHeap) root
    | root < 0 = heap
    | root >= hLength = heap
    | largest == left = maxHeapify (swapVals heap (getLeft root) root) (getLeft root)
    | largest == right = maxHeapify (swapVals heap (getRight root) root) (getRight root)
    | otherwise = heap
    where
        left = getVal heap (getLeft root)
        right = getVal heap (getRight root)
        current = getVal heap root
        leftRightLargest = if left < right then right else left
        largest = if leftRightLargest < current then current else leftRightLargest

listToHeap :: (Ord o) => [o] -> HeapType -> BinHeap o
listToHeap [] heapType = newHeap heapType
listToHeap list heapType = foldl (flip heapAdd) (newHeap heapType) list

{-
printHeap :: (Ord o) => BinHeap o -> String
printHeap heap = printHeapHelp heap 0 ""

printHeapHelp :: (Show o) => BinHeap o -> Int -> String -> String
printHeapHelp heap@(BinHeap list hLength _) root tabs
    | root >= hLength = ""
    | otherwise = tabs ++ (show (getVal heap root)) ++ "\n" ++ printHeapHelp heap (getLeft root) (tabs ++ "\t") ++ printHeapHelp heap (getRight root) (tabs ++ "\t")
-}