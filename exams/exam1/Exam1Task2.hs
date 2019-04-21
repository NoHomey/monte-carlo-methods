{-# LANGUAGE FlexibleContexts #-}

module Exam1Task2 (
    Vector,
    Matrix,
    task2
) where

import qualified Matrix as M
import qualified BinaryTree as BinTree
import Instances
import Data.Maybe

type Vector a = BinTree.Tree Int a

type Matrix a = M.Matrix (BinTree.Tree Int) a

zeros :: Int -> [[Rational]]
zeros n = replicate n $ replicate n 0

task2 :: Matrix Rational -> Vector Rational -> Matrix Rational
task2 qM p = let n = foldr (\_ a -> a + 1) 0 p
                 zero = M.matrix $ M.matrixConvert $ M.Matrix $ zeros n
                 withoutDiag = BinTree.indexMap fillRow zero
             in M.Matrix $ BinTree.indexMap fillDiag withoutDiag
    where fillRow (i, row) = BinTree.indexMap (\(j, e) -> if i == j
                                                            then e
                                                            else let pi_i = pi i
                                                                     pi_j = pi j
                                                                     q_ij = q i j
                                                                     q_ji = q j i
                                                                     a = if q_ij == 0
                                                                           then 1
                                                                           else (pi_j / pi_i) * (q_ji / q_ij)
                                                                 in q_ij * (min 1 a)
                                              ) row
          pi k = fromJust $ BinTree.find p k
          q l k = fromJust $ M.element qM (l, k)
          fillDiag (i, row) = BinTree.insert row i $ 1 - (sum row)
        

