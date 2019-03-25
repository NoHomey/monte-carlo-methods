{-# LANGUAGE MultiParamTypeClasses #-}

module Matrix where

import Control.Monad
import Data.List

newtype Matrix c e = Matrix {matrix :: c (c e)}

class Convertion from to where
    convert :: from e -> to e

class Indexable c i where
    atIndex :: c e -> i -> Maybe e 

matrixConvert :: (Convertion c s, Functor c) => Matrix c e -> Matrix s e
matrixConvert = Matrix . convert . fmap convert . matrix

element :: (Integral i, Indexable c i) => Matrix c e -> (i, i) -> Maybe e
element m (i, j) = join $ fmap (\row -> atIndex row j) $ atIndex (matrix m) i

powerUp :: (Num a) => Matrix [] a -> Int -> Matrix [] a
powerUp m n = if n < 2
                then m
                else let rows = matrix m
                         cols = transpose rows
                     in power n rows cols
    where power n curr cols = if n == 1
                               then Matrix curr
                               else power (n - 1) (mult curr cols) cols
          mult curr cols = map (\row -> map (\col -> sum $ zipWith (*) row col) cols) curr
