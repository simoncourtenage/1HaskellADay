module HAD.Y2014.M03.D19.Exercise where

import Data.List (group)

-- $setup
-- >>> import Control.Applicative ((<*>))
-- >>> import Data.List (isInfixOf)
-- >>> import Test.QuickCheck

-- Level: Easy
-- Pointfree: yes


-- | mostRepeatedElem
-- Returns the element with the longest (consecutive) repetition and the
-- repetition number
-- If there are tie, the last most repeated element is returned
-- It returns error on empty string
-- 
-- Examples:
--
-- >>> mostRepeatedElem "hello world!"
-- ('l',2)
--
-- >>> mostRepeatedElem [1,1,2,2]
-- (2,2)
--
-- prop> (flip isInfixOf <*> uncurry (flip replicate) . mostRepeatedElem) . getNonEmpty

{--
  This is a kind of hand-coded version of the official solution (I like to think!). Especially the
  expression 'map (\x -> (head x,length x))' compared with 'map (head &&& length)'.
--}

mostRepeatedElem :: Eq a => [a] -> (a,Int)
mostRepeatedElem = (\xs -> foldr f (head xs) $ tail xs) . map (\x -> (head x,length x)) . group
    where f (a,b) (z,l) | b >= l    = (a,b)
                        | otherwise = (z,l)