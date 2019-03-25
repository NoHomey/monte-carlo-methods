{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Instances where

import qualified Matrix as M
import qualified BinaryTree as BinTree
import Data.Foldable
import Data.List

instance (Integral i) => M.Convertion [] (BinTree.Tree i) where
    convert = BinTree.listToTree

instance M.Convertion (BinTree.Tree k) [] where
    convert = toList

instance (Integral i) => M.Indexable (BinTree.Tree i) i where
    atIndex = BinTree.find

instance (Integral i) => M.Indexable [] i where
    atIndex l i = let len = genericLength l
                  in if (i > 0) && (i <= len)
                       then Just $ genericIndex l (i - 1)
                       else Nothing