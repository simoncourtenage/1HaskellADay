module HAD.Y2014.M03.D27.Exercise where

import Data.Maybe (catMaybes)
import Control.Arrow ((&&&))
import Data.List(group)

-- | compact Compact a list of values with the following rules:
-- - Nothing are removed
-- - If two remaining onsecutive values are equal,
--     they are replaced by one value equal to the successor of those values
-- - the previous rule is not recursive
-- - Other values are kept
--
-- Examples
--
-- >>> compact [Just 1, Nothing, Just 1, Just 2, Just 4, Just 3, Just 3]
-- [2,2,4,4]
--
-- >>> compact [Nothing, Nothing, Nothing]
-- []
--
-- >>> compact []
-- []
--
-- >>> compact $ map Just "hello"
-- "hemo"
--
-- prop> [succ x] == (compact . replicate 2 . Just) (x :: Int)
-- prop> [succ x, x] == (compact . replicate 3 . Just) (x :: Int)
-- prop> replicate 2 (succ x) == (compact . replicate 4 . Just) (x :: Int)
--

{--
  While this solution looks neater than the official solution, its performance is probably worse.  There
  are a lot of list concats going on here - which is rarely a good idea.
--}

compact :: (Enum a, Eq a) => [Maybe a] -> [a]
compact m = f =<< (fmap  (head &&& length) . group . catMaybes) m
    where f (n,m) | m == 1 = [n]
                  | otherwise = replicate (m `div` 2) (succ n) ++ replicate (m `mod` 2) n
