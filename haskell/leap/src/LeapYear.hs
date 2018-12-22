module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
-- isLeapYear year = error "You need to implement this function."
isLeapYear y = (isDivBy4 y && (not $ isDivBy100 y)) || isDivBy400 y
    where
      isDivBy4 n = n `mod` 4 == 0
      isDivBy100 n = n `mod` 100 == 0
      isDivBy400 n = n `mod` 400 == 0
