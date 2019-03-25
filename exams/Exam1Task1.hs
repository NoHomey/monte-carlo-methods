module Exam1Task1 (
    Square(..),
    Probs,
    increase,
    squareMapMatrix,
    task1A,
    task1B
) where

import qualified Matrix as M
import qualified BinaryTree as BinTree
import Instances
import Data.Maybe

data Square = Square
            { index :: Int
            , left :: Maybe Int
            , top :: Maybe Int
            , right :: Maybe Int
            , bottom :: Maybe Int
            }

type Probs = (Rational, Rational, Rational, Rational)

increase :: Square -> Square
increase (Square i l t r b) = Square (i + 1) (iN l) (iN t) (iN r) (iN b)
    where iN = fmap (+1)

squareMapMatrix :: [Square] -> Probs -> M.Matrix (BinTree.Tree Int) Rational
squareMapMatrix info probs = let n = length info
                                 zs = M.matrixConvert $ M.Matrix $ zeros n
                                 ns = foldr (setNeighbours probs) (M.matrix zs) info
                             in M.Matrix $ BinTree.indexMap setStay ns

zeros :: Int -> [[Rational]]
zeros n = replicate n $ replicate n 0

setNeighbours :: Probs -> Square -> BinTree.Tree Int (BinTree.Tree Int Rational) -> BinTree.Tree Int (BinTree.Tree Int Rational)
setNeighbours (lP, tP, rP, bP) square mat = let rowIndex = index square
                                                row = fromJust $ BinTree.find mat rowIndex
                                            in BinTree.insert mat rowIndex $ set row square
    where set row square = let update1 = setNeighbour row (left square) lP
                               update2 = setNeighbour update1 (top square) tP
                               update3 = setNeighbour update2 (right square) rP
                               update4 = setNeighbour update3 (bottom square) bP
                           in update4

setNeighbour :: BinTree.Tree Int Rational -> Maybe Int -> Rational -> BinTree.Tree Int Rational
setNeighbour row maybeIndex prob = maybe row (\i -> BinTree.insert row i prob) maybeIndex
            
setStay :: (Int, BinTree.Tree Int Rational) -> BinTree.Tree Int Rational
setStay (i, row) = BinTree.insert row i $ 1 - (sum row)

task1A :: M.Matrix (BinTree.Tree Int) Rational -> M.Matrix [] Rational
task1A = M.matrixConvert

task1B :: M.Matrix [] Rational -> Int -> (Int, Int) -> Rational
task1B m pow (start, end) = fromJust $ M.element (M.powerUp m pow) (start + 1, end + 1)