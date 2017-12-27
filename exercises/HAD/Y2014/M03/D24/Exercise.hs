module HAD.Y2014.M03.D24.Exercise where

-- | squareList builds a list of x lists of size x from a given list of elements
-- If there aren't enough elements, fill the square with the second parameter
-- Examples:
--
-- >>> squareList 2  0 [0..]
-- [[0,1],[2,3]]
--
-- >>> squareList 2 0 [1]
-- [[1,0],[0,0]]
--
-- >>> squareList 3 () $ repeat ()
-- [[(),(),()],[(),(),()],[(),(),()]]
--

{--
  Feel somewhat humbled after looking at the solution :(
--}
squareList :: Int -> a -> [a] -> [[a]] 
squareList n r xs = f n n $ xs ++ repeat r
    where f 0 _ _ = []
          f n m l = take m l : f (n-1) m (drop m l)

