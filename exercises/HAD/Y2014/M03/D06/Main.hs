module Main where

import Control.Applicative

-- | takeStrictlyLessThan take elements of a list while their sum is
-- _strictly_ less than a given number
--
-- Point-free: I didnt' try without parameter, you can easily "hide" the 2nd
-- parameter (ie. takeStrictlyLessThan x = …)
-- Level: MEDIUM
--
-- Examples:
-- >>> takeStrictlyLessThan (10::Int) [1..]
-- [1,2,3]
-- 
-- >>> takeStrictlyLessThan (3::Integer) $ repeat 1
-- [1,1]
--
-- >>> takeStrictlyLessThan (42::Int) $ []
-- []
--
takeStrictlyLessThan :: (Num a, Ord a) => a -> [a] -> [a]
takeStrictlyLessThan a xs = f 0 a xs
    where f _ _ [] = []
          f c t (y:ys) | c + y < t = y : f (c+y) t ys
                       | otherwise = []

main = do
    let result = takeStrictlyLessThan (9999999999::Int) [1..]
    let total = sum result
    putStrLn ("result is " ++ show total)
