-------------------------------------------------------------------------------
-- Student's name: Krzysztof Klimek
--
-- Maxiphobic Heaps
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Heap.MaxiphobicHeap
  ( Heap
  , empty
  , isEmpty
  , minElem
  , delMin
  , insert
  , merge
  , mkHeap
  ) where

import Test.QuickCheck

data Heap a  = Empty | Node a Int (Heap a) (Heap a) deriving Show

-- number of elements in tree rooted at node
weight :: Heap a -> Int
weight Empty           = 0
weight (Node _ w _ _)  = w


singleton :: a -> Heap a
singleton x  = Node x 1 Empty Empty


empty :: Heap a
empty = Empty

isEmpty :: Heap a -> Bool
isEmpty empty = True
isEmpty _ = False

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (singleton x) h

minElem :: Heap a -> a
minElem empty = error "error"
minElem (Node x _ _ _) = x

delMin :: (Ord a) => Heap a -> Heap a
delMin empty = error "error"
delMin (Node _ _ lh rh) = merge lh rh

node :: a -> Heap a -> Heap a -> Heap a
node x h h'
| w >= w'    = Node x s h h'
| otherwise  = Node x s h' h
where
  w = weight h
  w' = weight h'
  s = w + w' + 1


sort3 :: (Ord a) => Heap a -> Heap a -> Heap a -> (Heap a, Heap a, Heap a)
sort3 h h' h''
  | weight h >= weight h' && weight h >= weight h'' && weight h' >= weight h'' = (h, h', h'')
  | weight h >= weight h' && weight h >= weight h'' && weight h'' >= weight h' = (h, h'', h')

  | weight h' >= weight h && weight h' >= weight h'' && weight h >= weight h'' = (h', h, h'')
  | weight h' >= weight h && weight h' >= weight h'' && weight h'' >= weight h = (h', h'', h)

  | weight h'' >= weight h && weight h'' >= weight h' && weight h >= weight h = (h'', h, h')
  | weight h'' >= weight h && weight h'' >= weight h' && weight h' >= weight h = (h'', h', h)

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty h'     = h'
merge h     Empty  = h
merge h@(Node x w lh rh) h'@(Node x' w' lh' rh')
 | x <= x'         = node x h11 (merge h12 h13)
 | otherwise       = node x' h21 (merge h22 h23)
 where
   (h11, h12, h13) = sort3 lh rh h'
   (h21, h22, h23) = sort lh' h rh'



--merge :: (Ord a) => Heap a -> Heap a -> Heap a
--merge Empty h'     = h'
--merge h     Empty  = h
--merge h@(Node x w lh rh) h'@(Node x' w' lh' rh')
-- | x <= x'         = node x lh (merge rh h')
-- | otherwise       = node x' lh' (merge h rh')



-- Efficient O(n) bottom-up construction for heaps
mkHeap :: (Ord a) => [a] -> Heap a
mkHeap []  = empty
mkHeap xs  = mergeLoop (map singleton xs)
  where
    mergeLoop [h]  = h
    mergeLoop hs   = mergeLoop (mergePairs hs)

    mergePairs []         = []
    mergePairs [h]        = [h]
    mergePairs (h:h':hs)  = merge h h' : mergePairs hs

-------------------------------------------------------------------------------
-- Generating arbritray Heaps
-------------------------------------------------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary  = do
    xs <- arbitrary
    return (mkHeap xs)
