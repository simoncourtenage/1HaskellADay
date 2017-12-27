module HAD.Y2014.M04.D03.Exercise where

-- | foo
-- Types. Powerful enough to get it right.
--
foo :: (a ->  b) -> [a] -> [(a,b)]
foo f = map (\x -> (x,f x))
