module HAD.Y2014.M02.D27.Exercise where

-- | Divide all the elements of the list (2nd parameter) by the first parameter
-- iff all the elements of the lists are exact multiple of it
-- returns nothing otherwise 
--
-- Examples:
--
-- >>> divIfMultiple 3 [3, 6 .. 12]
-- Just [1,2,3,4]
-- >>> divIfMultiple 2 [3, 6 .. 12]
-- Nothing
--

-- I must admit I don't like the official solution to this exercise.
-- It's far too complex, and unless there's some advantage to it, for
-- example, for performance, I think this solution is better for being
-- a little clearer.
divIfMultiple :: Integral a => a -> [a] -> Maybe [a]
divIfMultiple x = sequence . (fmap f)
    where
        f a | a `mod` x == 0 = Just $ a `div` x
            | otherwise      = Nothing
