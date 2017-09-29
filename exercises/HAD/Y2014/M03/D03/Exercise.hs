module HAD.Y2014.M03.D03.Exercise where

import Data.List (sortBy)

-- | Sort a list of list of elements by the maximum of each list,
-- in ascending order
-- 
-- Point-free: easy and readable
-- Level: EASY
--
-- Examples:
-- >>> sortByMax [[1,10],[5,5]]
-- [[5,5],[1,10]]
-- >>> sortByMax []
-- []
-- 
-- sortByMax [[], [1,2]]
-- should throw an execption: no max for empty list

-- sortByMax :: Find the most generic signature

{--
  Funnily enough, this is my second attempt at solving this exercise.  My first attempt was the
  official solution, but I had included a Foldable type constraint.  When I ran this solution, a
  type error was reported when sortByMax was applied to [].  This confused me and made me think
  the code was wrong, when in fact the type constraint needed to be removed.
--}

sortByMax :: Ord a => [[a]] -> [[a]]
sortByMax l = map snd . sortBy (\a b -> compare (fst a) (fst b)) $ fmap (\x -> (maximum x,x)) l
