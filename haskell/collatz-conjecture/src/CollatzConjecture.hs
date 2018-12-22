module CollatzConjecture (collatz) where

  import Data.Maybe 

  xcollatz :: Integer -> Maybe Integer
  -- collatz = error "You need to implement this function."
  xcollatz n 
    | n <= 0 = Nothing
    |otherwise = Just $ steps n 0
    
  steps :: Integer -> Integer -> Integer
  steps n ac
    | n == 1 = ac
    | n `mod` 2 == 0 = steps (div n 2) $ (+) ac 1 -- steps (n `div` 2) ac+1
    | otherwise = steps (3*n+1) (ac+1) 

  collatz :: Integer -> Maybe Integer
  collatz n
      | n <= 0 = Nothing
      | n == 1 = Just 0
      | mod n 2 == 0 = fmap (+1) $ collatz (n `div` 2)
      | otherwise = fmap (+1) $ collatz (n*3+1)