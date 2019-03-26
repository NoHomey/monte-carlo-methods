module BinaryTree where

import Data.Maybe
import Data.List (genericLength)

data Tree k v =
    Empty |
    Node (k, v) (Tree k v) (Tree k v) deriving Show

instance Functor (Tree k) where
    fmap f = indexMap (\(_, val) -> f val)

instance Foldable (Tree k) where
    foldr f = indexFold (\(_, val) accum -> f val accum)

indexMap :: ((k, x) -> y) -> Tree k x -> Tree k y
indexMap _ Empty                             = Empty
indexMap f (Node info@(key, val) left right) = Node (key, f info) (indexMap f left) (indexMap f right)

indexFold :: ((k, v) -> a -> a) -> a -> Tree k v -> a
indexFold _ accum Empty                  = accum
indexFold f accum (Node info left right) = indexFold f (f info $ indexFold f accum right) left

intervalTree :: (Integral i) => i -> i -> Tree i (Maybe v)
intervalTree a b | a == b    = Node (a, Nothing) Empty Empty
                 | b < a     = Empty
                 | otherwise = let mid = a + (b - a) `div` 2
                               in Node (mid, Nothing) (intervalTree a $ mid - 1) (intervalTree (mid + 1) b)

find :: (Ord k) => Tree k v -> k ->  Maybe v
find Empty                            _   = Nothing
find (Node (rootKey, val) left right) key = case compare key rootKey of
                                                EQ -> Just val
                                                LT -> find left key
                                                GT -> find right key

insert :: (Ord k) => Tree k v -> k -> v -> Tree k v
insert Empty                               key val = Node (key, val) Empty Empty
insert (Node root@(rootKey, _) left right) key val = case compare key rootKey of
                                                         EQ -> Node (key, val) left right
                                                         LT -> Node root (insert left key val) right
                                                         GT -> Node root left $ insert right key val

listToTree :: (Integral i) => [v] -> Tree i v
listToTree l = let len = genericLength l
                   searchTree = intervalTree 1 len
               in fmap fromJust $ snd $ foldl update (1, searchTree) l
    where update (i, tree) val = (i + 1, insert tree i $ Just val)