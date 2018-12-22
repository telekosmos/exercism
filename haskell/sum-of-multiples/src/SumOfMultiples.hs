module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors l = foldr (+) 0 $ multiplos factors [1..l-1]
  where
    multiplos f l = [ x | x <- l, any (\c -> x `mod` c == 0) f]

------------------

isMultiple :: [Integer] -> Integer -> Bool
isMultiple [] _ = False
isMultiple (x:xs) n
  | n `mod` x == 0 = True
  | otherwise = isMultiple xs n

esMultiplo :: [Integer] -> Integer -> Bool
esMultiplo factors n = or $fmap (\c -> n `mod` c == 0) factors

-- comprehension list containing elements of l which is multiple of ANY factor
multiplos :: [Integer] -> [Integer] -> [Integer]
multiplos factors l = [ x | x <- l, any (\c -> x `mod` c == 0) factors]

multiples :: [Integer] -> [Integer] -> [Integer]
multiples factors l = filter (isMultiple factors) l
  