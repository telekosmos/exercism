module Squares (difference, squareOfSum, sumOfSquares) where


difference :: Integral a => a -> a
difference = fmap (-) squareOfSum <*> sumOfSquares

squareOfSum :: Integral a => a -> a
squareOfSum n = (^) (foldr (+) 0 [1..n]) 2

sumOfSquares :: Integral a => a -> a
sumOfSquares n = foldr (+) 0 [ x | x <- fmap (\c -> (^) c 2) [1..n]]
