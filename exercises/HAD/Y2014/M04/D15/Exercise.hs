module HAD.Y2014.M04.D15.Exercise where

{- | lowestFreeInt
   Find the lowest non-negative integer not in the list
  
   (Thanks to @clementd for this one)

   Example:

   >>> lowestFreeInt [0..10]
   11

   >>> lowestFreeInt [1..10]
   0

   >>> lowestFreeInt $ [0..998] ++ [1000..9999]
   999

-}

lowestFreeInt :: [Int] -> Int
lowestFreeInt [] = 0
lowestFreeInt xss@(x:xs)
    | x > 0                            = last (take x [0..])
    | length xss == (last xss - (x-1)) = last xss + 1
    | otherwise                        = (+1) . fst . head . filter f $ zip xss xs
    where f (a,b) | b == a + 1 = False
                  | otherwise  = True
