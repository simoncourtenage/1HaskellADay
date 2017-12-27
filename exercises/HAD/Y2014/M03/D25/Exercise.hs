module HAD.Y2014.M03.D25.Exercise
  ( Board
  , board
  , getList
  ) where

import Data.List (groupBy)
import Test.QuickCheck

-- Preamble

-- $setup
-- >>> import Control.Applicative ((<$>), (<*>))
-- >>> import Data.List (group)

-- A board is a "square list of list"
-- The "square" form, is ensure by the constructor (board)
newtype Board a = Board {getList :: [[a]]}
  deriving (Eq, Show, Read)

-- | Exercise
-- Build an Arbitrary instance for Board of any given size
--
-- The external list length is equal to the length of each internal lists
-- prop> (1==) . length . group . (flip (:) <*> length) . map length $ getList bs
--

{--
  I like the official solution - there's a lot to be learnt from it.  On the other hand, it's also a 
  bit puzzling.  What exactly is it testing?  If the 'square' nature of the Board is due to 'board',
  then shouldn't you test Boards built by 'board'?  So why not use 'board' in arbitrary?
--}

instance Arbitrary a => Arbitrary (Board a) where 
  arbitrary = do 
    n <- arbitrary -- board size
    v <- arbitrary -- default value
    l <- arbitrary -- list of values
    return $ Board (board n v l)




-- Just some extra content, it isn't useful for today's exercise

-- Constructor

-- | board Yesterday's squareOf, build a square board with initial values
board :: Int -> a -> [a] -> [[a]] 
board n x = take n . map (take n) . iterate (drop n) . (++ repeat x)
