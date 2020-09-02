import Data.List
-- TreeNode has (Key, order, Parent, Sibling, Child) in that order
data BinoTree o = EmptyNode | TreeNode {key :: o, order :: Int, parent :: (BinoTree o), sibling :: (BinoTree o), child :: (BinoTree o)} deriving (Read, Eq)
data BinoHeap o = BinoHeap {tree :: (BinoTree o), heapTop :: Maybe o} deriving (Read, Eq)

instance (Ord o) => Ord (BinoTree o) where
    a < b = key a < key b
    a > b = key a > key b
    a <= b = key a <= key b
    a >= b = key a >= key b
    a `compare` b = (key a) `compare` (key b)
    max a b = if a > b then a else b
    min a b = if a < b then a else b

instance (Show o) => Show (BinoTree o) where
    show a = treePrintRaw a

instance (Show o) => Show (BinoHeap o) where
    show heap@(BinoHeap tree _) = treePrintRaw tree

heapNew :: (Ord o) => BinoHeap o
heapNew = BinoHeap EmptyNode Nothing

heapSingle :: (Ord o) => o -> BinoHeap o
heapSingle val = BinoHeap (treeNew val) (Just val)

treeNew :: (Ord o) => o -> BinoTree o
treeNew key = TreeNode key 0 EmptyNode EmptyNode EmptyNode

treeMerge :: (Ord o) => BinoTree o -> BinoTree o -> BinoTree o
treeMerge t1@(TreeNode k1 d1 p1 s1 c1) t2@(TreeNode k2 d2 p2 s2 c2)
    | k1 > k2 = treeMergeHelp t2 t1
    | otherwise = treeMergeHelp t1 t2

treeSetParent :: (Ord o) => BinoTree o -> BinoTree o -> BinoTree o
treeSetParent tree@(TreeNode tK tD _ tS tC) parentTree = TreeNode tK tD parentTree tS tC

treeSetChild :: (Ord o) => BinoTree o -> BinoTree o -> BinoTree o
treeSetChild tree@(TreeNode tK tD tP tS _) childTree = TreeNode tK tD tP tS childTree

treeSetSibling :: (Ord o) => BinoTree o -> BinoTree o -> BinoTree o
treeSetSibling tree@(TreeNode tK tD tP _ tC) siblingTree = TreeNode tK tD tP siblingTree tC

treeMergeHelp :: (Ord o) => BinoTree o -> BinoTree o -> BinoTree o
treeMergeHelp smaller@(TreeNode sK sD sP sS sC) larger@(TreeNode lK lD lP lS lC) = newSmaller
    where 
        newSmaller = TreeNode sK (sD + 1) sP lS newLarger
        newLarger = TreeNode lK lD newSmaller sC lC

treePrintRaw :: (Show o) => BinoTree o -> String
treePrintRaw tree = treePrintHelp tree 0

treePrint :: (Show o) => BinoTree o -> IO ()
treePrint tree = putStr $ treePrintHelp tree 0

treePrintHelp :: (Show o) => BinoTree o -> Int -> String
treePrintHelp EmptyNode _ = ""
treePrintHelp tree@(TreeNode tK _ _ tS tC) tabs = treePrintSingle tree tabs ++ treePrintHelp tC (tabs + 1) ++ treePrintHelp tS tabs

treePrintSingle :: (Show o) => BinoTree o -> Int -> String
treePrintSingle EmptyNode _ = ""
treePrintSingle tree@(TreeNode tK tD _ _ _) tabs = (concat . take tabs . repeat $ "\t") ++ show tD ++ "[" ++ show tK ++ "]" ++ "\n"

heapMerge :: (Ord o) => BinoHeap o -> BinoHeap o -> BinoHeap o
heapMerge heap1 heap2 = BinoHeap (heapMergeHelp (tree heap1) (tree heap2)) (min (heapTop heap1) (heapTop heap2))

heapMergeHelp :: (Ord o) => BinoTree o -> BinoTree o -> BinoTree o
heapMergeHelp tree EmptyNode = tree
heapMergeHelp EmptyNode tree = tree
heapMergeHelp tree1 tree2
    | (order tree1) < (order tree2) = treeSetSibling tree1 (heapMergeHelp (sibling tree1) tree2)
    | (order tree2) > (order tree1) = treeSetSibling tree2 (heapMergeHelp tree1 (sibling tree2) )
    | otherwise = treeSetSibling tree1 (heapMergeHelp (sibling tree1) tree2)

heapUnion :: (Ord o) => BinoHeap o -> BinoHeap o -> BinoHeap o
heapUnion (BinoHeap _ Nothing) heap = heap
heapUnion heap (BinoHeap _ Nothing) = heap
heapUnion heap1 heap2 = BinoHeap (heapUnionHelp x xNext) (heapTop merged)
    where
        merged = heapMerge heap1 heap2
        x = tree merged
        xNext = sibling x

heapUnionHelp :: (Ord o) => BinoTree o -> BinoTree o -> BinoTree o
heapUnionHelp tree EmptyNode = tree
heapUnionHelp x xNext
    |   (order x) /= (order xNext) = treeSetSibling x $ heapUnionHelp xNext (sibling xNext)
    |   sibling xNext == EmptyNode = treeSetSibling (treeMerge x xNext) EmptyNode
    |   (order x) == (order (sibling xNext)) = treeSetSibling x $ heapUnionHelp xNext (sibling xNext)
    |   otherwise = heapUnionHelp (treeMerge x xNext) (sibling xNext)

heapPush :: (Ord o) => o -> BinoHeap o -> BinoHeap o
heapPush val heap@(BinoHeap EmptyNode _) = heapSingle val
heapPush val heap = heapUnion (heapSingle val) heap

heapFromList :: (Ord o) => [o] -> BinoHeap o
heapFromList [] = heapNew
heapFromList list = foldl (flip heapPush) heapNew list