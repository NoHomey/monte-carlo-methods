module Main where

import Exam1Task1
import Exam1Task2
import qualified Matrix as M
import qualified BinaryTree as BinTree
import Data.Ratio
import Data.List

info :: [Square]
info = map increase
    [ Square { index = 0
             , left = Nothing
             , top = Just 1
             , right = Just 2
             , bottom = Nothing
             }
    , Square { index = 1
             , left = Nothing
             , top = Nothing
             , right = Nothing
             , bottom = Just 0
             }
    , Square { index = 2
             , left = Just 0
             , top = Nothing
             , right = Just 3
             , bottom = Nothing
             }
    , Square { index = 3
             , left = Just 2
             , top = Just 4
             , right = Nothing
             , bottom = Nothing
             }
    , Square { index = 4
             , left = Nothing
             , top = Just 5
             , right = Nothing
             , bottom = Just 3
             }
    , Square { index = 5
             , left = Nothing
             , top = Nothing
             , right = Nothing
             , bottom = Just 4
             }
    ]

probs :: Probs
probs = (1 % 10, 1 % 2, 1 % 5, 1 % 5)

indexedMatrix :: M.Matrix (BinTree.Tree Int) Rational
indexedMatrix = squareMapMatrix info probs

matrix :: M.Matrix [] Rational
matrix = task1A indexedMatrix

showQ :: String -> Rational -> String
showQ del q = let a = numerator q
                  b = denominator q
              in (if b == 1 then show a else (show a) ++ "/" ++ (show b)) ++ del

printMatrix :: (Foldable t) => M.Matrix t Rational -> IO ()
printMatrix m = mapM_ showLine $ M.matrix m
    where showLine line = (mapM_ (putStr . (showQ "\t\t")) line) >> (putChar '\n')

printMatrixForMathematica :: (Foldable t) => M.Matrix t Rational -> IO ()
printMatrixForMathematica m =  (putChar '{') >> (mapM_ showLine $ M.matrix m) >> (putStrLn "}")
    where showLine line = (putChar '{') >> (mapM_ (putStr . (showQ ", ")) line) >> (putStr "}, ")

p :: Vector Rational
p = BinTree.listToTree [5, 10, 5, 10, 25, 60]

q :: Matrix Rational
q = squareMapMatrix info (1 % 4, 1 % 4, 1 % 4, 1 % 4)

a :: M.Matrix [] Rational
a = M.matrixConvert $ M.Matrix $ BinTree.indexMap (\(i, row) -> BinTree.indexMap (\(j, e) -> if i == j then e - 1 else e) row) $ M.matrix indexedMatrix

at :: M.Matrix [] Rational
at = M.Matrix $ transpose $ M.matrix a

main = do
         printMatrix matrix
         putStrLn ""
         printMatrixForMathematica at
         putStrLn ""
         putStrLn $ showQ "" $ task1B matrix 2 (2, 4)
         putStrLn ""
         putStrLn $ showQ "" $ task1B matrix 3 (2, 4)
         putStrLn ""
         printMatrix $ task2 q p
