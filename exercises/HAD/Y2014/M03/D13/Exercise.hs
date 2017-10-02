module HAD.Y2014.M03.D13.Exercise where

import Data.Monoid ((<>))

-- | pairToList Transform a pair of same type elements in a list of two
-- elements.
-- 
-- Of course, the major challenge is to find a point free function
-- (without lambda). And, if you want more fun, do it without (++).
--
-- prop> replicate 2 (x :: Int) == pairToList (x,x)
--
-- prop> (\(f,s) -> [f,s]) x == pairToList x 
--

{--
  The official solution was what I had tried for, but I couldn't work out the LHS of <*>.  In the end,
  I came up with this, which uses the <> over lists to perform concat (so is a bit of a cheat!)
--}

pairToList :: (a,a) -> [a]
pairToList = (<>) . (:[]) . fst <*> (:[]) . snd
