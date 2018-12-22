module Pangram (isPangram) where

  import Data.List
  import Data.Char

  pangram :: String -> [Char] -> Bool
  pangram _ [] = True
  pangram [] l = False
  pangram (x:xs) l = pangram xs $ delete x l
  
  isPangram :: String -> Bool
  isPangram text = let alphabet = ['a'..'z'] in
    pangram (map toLower text) alphabet
