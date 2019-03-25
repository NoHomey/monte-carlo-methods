module BinaryTree where

import Data.Maybe
import Data.List (genericLength)

data Tree k v =
    Empty |
    Node (k, v) (Tree k v) (Tree k v) deriving Show

instance Functor (Tree k) where
    fmap _ Empty                        = Empty
    fmap f (Node (key, val) left right) = Node (key, f val) (fmap f left) (fmap f right)

instance Foldable (Tree k) where
    foldr _ accum Empty                      = accum
    foldr f accum (Node (_, val) left right) = foldr f (f val $ foldr f accum right) left

indexMap :: ((k, x) -> y) -> Tree k x -> Tree k y
indexMap _ Empty                        = Empty
indexMap f (Node (key, val) left right) = Node (key, f (key, val)) (indexMap f left) (indexMap f right)

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