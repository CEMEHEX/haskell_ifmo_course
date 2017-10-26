module Block3.Task5 where

import           Block3.TreePrinters
import           System.Random       (newStdGen, randomRs)

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

treeSize :: (Integral n) => Tree a -> n
treeSize Leaf         = 0
treeSize (Node _ l r) = 1 + treeSize l + treeSize r

inTree :: (Ord a) => a -> Tree a -> Bool
inTree _ Leaf = False
inTree val (Node a l r) | val == a = True
                        | val < a = inTree val l
                        | otherwise = inTree val r

insert :: (Ord a) => a -> Tree a -> Tree a
insert val Leaf              = Node val Leaf Leaf
insert val tree@(Node a l r) | val == a = tree
                             | val < a = Node a (insert val l) r
                             | otherwise = Node a l $ insert val r

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert Leaf

testTree :: (Num a) => Tree a
testTree = Node 3 (Node 1 Leaf Leaf) $ Node 5 (Node 4 Leaf Leaf) (Node 123 Leaf Leaf)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen
