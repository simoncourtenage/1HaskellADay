module HAD.Y2014.M03.D07.Exercise where

import Data.List (replicate)

-- | trueIndexes produce an infinite list where only the index given in the list
-- in parameter are true.
-- The parameter list is supposed to be sorted and nubbed
--
-- Point-free: Probably hard to find!
-- Level: HARD
--
-- Examples:
-- >>> take 2 $ trueIndexes [1]
-- [False,True]
-- 
-- >>> take 6 $ trueIndexes [0,2..]
-- [True,False,True,False,True,False]
--
-- >>> take 3 $ trueIndexes []
-- [False,False,False]
--

-- my version - bit annoyed that it's not as one-linery as the solution, and that I didn't spot the obvious
-- need for intercalate.
-- Note also that I've also edited the result of the last test above (take 3 $ trueIndexes []) to remove
-- the spaces after the commas - this was throwing the testing
trueIndexes :: [Int] -> [Bool]
trueIndexes [] = repeat False
trueIndexes l =  concat . fmap (\a -> replicate (a-1) False ++ [True]) $ head l + 1: (zipWith (flip (-)) <*> tail) l

