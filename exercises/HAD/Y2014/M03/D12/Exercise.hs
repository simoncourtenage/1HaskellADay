module HAD.Y2014.M03.D12.Exercise where

import Control.Applicative

-- | localMax Given an entry list, it outputs the its list of local maxima.
-- A Local maximum is a an element greater than its predecessor and than its
-- successor.
--
-- Examples:
--
-- >>> localMax [0 .. 1000]
-- []
--
-- >>> localMax [1000 .. 0]
-- []
--
-- >>> localMax [2,2,1,5,4]
-- [5]
--
-- >>> take 4 . localMax $ [0..] >>= (\y -> [y,y+2])
-- [2,3,4,5]
--

{--
  Not a great solution.  The basic idea was to group the input list into 3-tuples and filter out those
  whose middle element was greater than the first and third element.  Then extract those middle elements
  into a list.
--}
localMax :: Ord a => [a] -> [a]
localMax [] = []
localMax xs = (filter (\(a,b,c) -> b > a && b > c) $
                getZipList $ (,,) <$> ZipList (init xs)
                                    <*> ZipList (tail . init $ xs)
                                    <*> ZipList (tail . tail $ xs))
                >>= \(a,b,c) -> [b]
