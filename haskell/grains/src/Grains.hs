module Grains (square, total) where

import Data.Maybe

square :: Integer -> Maybe Integer
square n 
  | n <= 0 || n > 64 = Nothing
  | n == 1 = Just 1
  | otherwise = (2*) <$> square (n-1)

total :: Integer
total = foldr (+) 0 $ fromJust <$> square <$> [1..64]
