module Acronym (abbreviate) where
import Data.Char

abbreviate :: String -> String
-- abbreviate xs = error "You need to implement this function."
-- words -> pick first char -> filter by alpha -> toUpper (inverse is composition)
abbreviate s = fmap toUpper $ filter isAlpha $ fmap head $ words s

-- HyperText -> ["Hyper", "Text"]
splitComma :: String -> [String]
splitComma "" = [""]
splitComma (',':cs) = "" : splitComma cs
splitComma (c:cs) = (c:cellCompletion) : otherCells
  where cellCompletion : otherCells = splitComma cs

splitWords :: String -> [String]
splitWords "" = [""]
splitWords (c:cs)
  | isUpper c = [c] ++ splitWords cs
  | otherwise = c:splitWords cs
