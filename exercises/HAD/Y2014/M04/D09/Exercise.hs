module HAD.Y2014.M04.D09.Exercise where

import Data.List (groupBy,sortBy)
import Data.Ord (compare)

-- $setup
-- >>> import Data.List

data Foo = Foo {x :: Int, y :: String, z :: String}
  deriving (Read, Show, Eq)

{- | orderXYZ
   Order Foo by x then by y and then by z
   
   prop> sort xs == (map x . orderXYZ . map (\v -> Foo v  "y" "z")) xs
   prop> sort xs == (map y . orderXYZ . map (\v -> Foo 42  v  "z")) xs
   prop> sort xs == (map z . orderXYZ . map (\v -> Foo 42 "y"  v )) xs
-}
orderXYZ :: [Foo] -> [Foo]
orderXYZ = concat . fmap concat .
            (fmap . fmap) (concat . sortFoo z) .
               fmap (sortFoo y) .
                  sortFoo x
  where eqFoo f  = \a b -> (==) (f a) (f b)
        cmpFoo f = \a b -> compare (f a) (f b)
        sortFoo f = sortBy (cmpFoo (f . head)) . groupBy (eqFoo f)

-- concat . fmap (sortFoo y) . 