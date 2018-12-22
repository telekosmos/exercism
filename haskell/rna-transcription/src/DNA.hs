module DNA (toRNA) where
import Data.Char
import Data.List

toRNA :: String -> Either Char String
-- toRNA xs = error "You need to implement this function."
-- toRNA "U" = Left 'U'
-- toRNA "utand" = Left 'X'
-- toRNA "acgggxyo" = Left 'X'

toRNA s
  | s == "" = Right ""
  | elem 'U' s == True = Left 'U'
  | (length $ filter (\c -> not $ elem c dnaAlpha) $ fmap toUpper s) > 0 = Left 'X'
  | otherwise = Right $ fmap (d2r . toUpper) s

dnaAlpha = "ACGT"

d2r :: Char -> Char
d2r 'A' = 'U'
d2r 'C' = 'G'
d2r 'G' = 'C'
d2r 'T' = 'A'
