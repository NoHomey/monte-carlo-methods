module Main where

import Exam1Task1
import Exam1Task2
import qualified Matrix as M
import qualified BinaryTree as BinTree
import Data.Ratio

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

showQ :: Rational -> String
showQ q = let a = numerator q
              b = denominator q
          in (if b == 1 then show a else (show a) ++ "/" ++ (show b)) ++ "\t\t"

printMatrix :: (Foldable t) => M.Matrix t Rational -> IO ()
printMatrix m = mapM_ showLine $ M.matrix m
    where showLine line = (mapM_ (putStr . showQ) line) >> (putChar '\n')

{-p :: Vector Rational
p = BinTree.listToTree [1 % 10, 1 % 5, 3 % 10, 2 % 5]

q :: Matrix Rational
q = M.matrixConvert $ M.Matrix
    [ [ 1 % 5, 1 % 5, 3 % 10, 3 % 10 ]
    , [ 1 % 5, 1 % 5, 3 % 10, 3 % 10 ]
    , [ 1 % 10, 1 % 10, 1 % 2, 3 % 10 ]
    , [ 2 % 5, 1 % 5, 1 % 10, 3 % 10 ]
    ]
-}

p :: Vector Rational
p = BinTree.listToTree [5, 10, 5, 10, 25, 60]

q :: Matrix Rational
q = squareMapMatrix info (1 % 4, 1 % 4, 1 % 4, 1 % 4)

main = do
         printMatrix matrix
         putStrLn ""
         putStrLn $ showQ $ task1B matrix 2 (2, 4)
         putStrLn ""
         putStrLn $ showQ $ task1B matrix 3 (2, 4)
         putStrLn ""
         printMatrix $ task2 q p
