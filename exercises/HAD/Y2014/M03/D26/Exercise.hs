module HAD.Y2014.M03.D26.Exercise
  ( Board
  , board
  , getList
  , Direction (..)
  , viewFrom
  ) where

import Data.List (groupBy,transpose)
import Control.Applicative ((<*>))
import Control.Monad (liftM, replicateM)

import Test.QuickCheck

-- Preamble

-- setup
import Control.Applicative ((<$>), (<*>))
import Data.List (sort)
--
checkReverse d1 d2 =
     (==) <$>
        sort . map sort . getList . viewFrom d1 <*>
        sort . map (sort . reverse) . getList . viewFrom d2 


newtype Board a = Board {getList :: [[a]]}
  deriving (Eq, Show)

data Direction = North | South | East | West
  deriving (Eq, Read, Show)

-- Exercise

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

{--
  Let's assume that a board of [[1,2,3],[4,5,6],[7,8,9]] is actually
           NORTH
      WEST 1 2 3   EAST
           4 5 6
           7 8 9
           SOUTH

  So if you view from the North, this list should be [[1,4,7],[2,5,8],[3,6,9]] and if from the East,
  then [[3,2,1],[6,5,4],[9,8,7]].  South will be [[7,4,1],[8,5,2],[9,6,3]].

  Further, we assume that any board passed to viewFrom is viewed from the West (see note above).
--}

viewFrom :: Direction -> Board a -> Board a
viewFrom West = id
viewFrom East = Board . fmap reverse . getList
viewFrom North = Board . transpose . getList
viewFrom South = Board . fmap reverse . transpose . reverse . getList


-- Constructor

-- | board Yesterday's squareOf, build a square board with initial values
board :: Int -> a -> [a] -> [[a]] 
board n x = take n . map (take n) . iterate (drop n) . (++ repeat x)

-- Arbitrary instances

instance Arbitrary a => Arbitrary (Board a) where 
  arbitrary = liftM Board (arbitrary >>= replicateM <*> vector)

instance Arbitrary Direction where
  arbitrary = elements [North, South , East , West]
