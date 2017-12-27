module HAD.Y2014.M03.D28.Exercise where

import Data.List (transpose)
import Data.Maybe (catMaybes)
import Control.Applicative ((<*>))
import Control.Monad (liftM, replicateM)

import Test.QuickCheck

-- $setup
-- >>> import Control.Applicative ((<$>), (<*>))
-- >>> import Data.List (sort)
-- >>> import Data.Maybe (catMaybes)
-- >>> :{
--   let
--    checkReverse d1 d2 =
--     (==) <$>
--        sort . map sort . getList . viewFrom d1 <*>
--        sort . map (sort . reverse) . getList . viewFrom d2 
-- :}

-- pushTo Push elements of the board to one direction, and compact them
-- (with a behavior similar to the one of the compact function defined
-- yesterday) and fill with Nothings to get a new Board
--
-- (yes, it is the same behavior as 2048):
-- http://gabrielecirulli.github.io/2048/
--
-- >>> getList . pushTo West $ Board [[Just 1, Just 1], [Just 2, Just 2]]
-- [[Just 2,Nothing],[Just 3,Nothing]]
--
-- >>> getList . pushTo North $ Board [[Just 1, Just 1], [Just 2, Just 2]]
-- [[Just 1,Just 1],[Just 2,Just 2]]
--
-- >>> getList . pushTo West $ Board [[Just 1, Just 1], [Just 2, Just 2]]
-- [[Just 2,Nothing],[Just 3,Nothing]]
--
-- prop> :{ \(d, bs) ->
--    (==) 
--    <$> map compact . getList . viewFrom d
--    <*> catMaybes . videFrom d . pushTo d
--     $  bs
-- :}

{--
    Algorithm:
      - change view to direction, so lists all go from left to right
      - map compact over each list
      - then apply Just to remaining values and pad out with Nothings
      - change view back

--}
pushTo :: (Enum a, Eq a) => Direction -> Board (Maybe a) -> Board (Maybe a)
pushTo d b = viewFrom d . Board . fmap (take (length (head . getList $ b)))
  $ fmap (foldr (:) (repeat Nothing))
    $ (fmap . fmap) Just
      $ map compact . getList $ viewFrom d b


-- Old stuff

newtype Board a = Board {getList :: [[a]]}
  deriving (Eq, Show)

data Direction = North | South | East | West
  deriving (Eq, Read, Show)


-- | viewFrom given a direction, produce an involution such that the
-- inner lists elements are ordered as if they were seen from that direction.
-- 
--
-- Examples: 
--
-- Defaut view is from West
-- prop> xs == viewFrom West xs
--
-- The function is an involution
-- prop> \(d,xxs) -> (==) <*> (viewFrom d . viewFrom d) $ (xxs :: Board Int)
--
-- Ordering properties from opposite side views (for inner lists elements
-- prop> checkReverse West  East  (xxs :: Board Int)
-- prop> checkReverse East  West  (xxs :: Board Int)
-- prop> checkReverse North South (xxs :: Board Int)
-- prop> checkReverse South North (xxs :: Board Int)
--
viewFrom :: Direction -> Board a -> Board a
viewFrom d = let
  go West  = id
  go East  = reverse . map reverse
  go North = transpose
  go South = reverse . map reverse . transpose
  in Board . go d . getList


-- | compact Compact a list of values with the following rules:
-- - Nothing are removed
-- - If two remaining onsecutive values are equal,
--     they are replaced by one value equal to the successor of those values
-- - previous replacement are not recursive
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
compact :: (Enum a, Eq a) => [Maybe a] -> [a]
compact = let
  compact' (x:y:xs) | x == y    = succ x : compact' xs
                    | otherwise = x : compact' (y:xs)
  compact' xs                   = xs
  in compact' . catMaybes

-- Arbitrary instances

instance Arbitrary a => Arbitrary (Board a) where 
  arbitrary = liftM Board (arbitrary >>= replicateM <*> vector)

instance Arbitrary Direction where
  arbitrary = elements [North, South , East , West]
