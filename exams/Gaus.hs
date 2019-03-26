module Gaus where

import qualified BinaryTree as BinTree
import Data.Maybe
import Data.Foldable

type Vector a = BinTree.Tree Int a

type Row = Vector Integer

type Column = Row

type Matrix = Vector Row

data Gaus = Gaus { matrix :: Matrix
                 , result :: Column
                 , rows :: Vector Bool
                 , columns :: Vector Bool
                 }

data ColumnWithIndex = ColumnWithIndex { index ::Int, column :: Column }

data ElementWithIndex = ElementWithIndex { i ::Int, e :: Integer }

instance Eq ColumnWithIndex where
    (==) = liftCompartion (==)

instance Ord ColumnWithIndex where
    (<=) = liftCompartion (<=)

instance Eq ElementWithIndex where
    l == r = (e l) == (e r)

instance Ord ElementWithIndex where
    l <= r = (e l) <= (e r)

gaus :: Matrix -> Column -> (Matrix, Column)
gaus a b = let m = length a
               n = length $ element a 1
               rowStatus = fromList $ replicate m False
               columnStatus = fromList $ replicate n False
               start = Gaus a b rowStatus columnStatus
               end = solve start
               a' = matrix end
               b' = result end
            in (a', b')

fromList :: [a] -> Vector a
fromList = BinTree.listToTree

sortedContent :: Column -> [Integer]
sortedContent col = toList $ foldr columnContent BinTree.Empty $ col
    where columnContent num curr = let absNum = abs num
                                   in BinTree.insert curr absNum absNum

liftCompartion :: (Integer -> Integer -> Bool) -> ColumnWithIndex -> ColumnWithIndex -> Bool
liftCompartion compare l r =  and $ zipWith (<=) (sortedContent $ column l) (sortedContent $ column r)

solve :: Gaus -> Gaus
solve curr = let unusedRows = unusedIndexes $ rows curr
                 unusedColumns = unusedIndexes $ columns curr
             in if null unusedRows || null unusedColumns
                  then curr
                  else let minColumn = minimum $ map selectColumn unusedColumns
                           columnContent = column $ minColumn
                           minElement = minimum $ map (\i -> ElementWithIndex i $ element columnContent i)  unusedRows
                       in solve $ step curr minElement minColumn
    where m = matrix curr
          selectColumn j = ColumnWithIndex { index = j
                                           , column = fmap (\row -> element row j) m
                                           }

unusedIndexes :: Vector Bool -> [Int]
unusedIndexes = indexFold (\(i, used) unused -> if used then i:unused else unused) []

element :: Vector a -> Int -> a
element vec i = fromJust $ BinTree.find vec i

indexFold :: ((Int, x) -> a -> a) -> a -> Vector x -> a
indexFold = BinTree.indexFold

indexMap :: ((Int, x) -> y) -> Vector x -> Vector y
indexMap = BinTree.indexMap